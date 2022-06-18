module Runtime.X64.BasicOps (
  ppop
, ppopW8
, ppopRestoreCallStack
, pdropW64
, pdropW8
, ppush  -- ^Parameter stack push
, ptopW8 -- ^Parameter stack top
, ppeek
, cpop  -- ^Call stack pop
, cpush -- ^Call stack push
, cdrop -- ^Call stack drop
, ctop
, cpeer
, cpeerW8
, defFunBasic
, rPstack
, rCallStack
, rDefBodies
, rDictIdx
, defineEmitW8
, defineEmitW64
, populateDictionaryKernel
, fnv1Hex
, fnv1Integral
)
where

import qualified ASM.Types as ASM
import qualified Data.Bits as B
import qualified Data.Int  as Int
import qualified Data.Word as Word
import qualified Text.Printf as Printf
import qualified X64.Types as X64
import qualified X64.X64   as X64

rPstack, rCallStack, rDefBodies, rDictIdx :: X64.Operand
rPstack    = X64.r13
rCallStack = X64.rsp
rDefBodies = X64.r15
rDictIdx   = X64.r14

-- String hash parameters
fnvPrime, fnvOffsetBasis :: Word.Word64
fnvPrime       = 0x100000001B3
fnvOffsetBasis = 0xCBF29CE484222325

fnvFold :: Bool -> Word.Word8 -> Word.Word64 -> Word.Word64
fnvFold False x h = (fnvPrime * h) `B.xor` fromIntegral x
fnvFold True  x h = fnvPrime * (h  `B.xor` fromIntegral x)

-- A helper function to compute a fnv-1 hash
fnv1 :: [Word.Word8] -> Word.Word64
fnv1 = foldr (fnvFold False) fnvOffsetBasis

fnv1s :: String -> Word.Word64
fnv1s = fnv1 . map X64.ascii

fnv1Integral :: String -> Word.Word64
fnv1Integral = fromIntegral . fnv1s

fnv1Hex :: Printf.PrintfType t => String -> t
fnv1Hex s = Printf.printf "%08X\n" $ fnv1s s

-- Parameter stack drop, alters the parameter stack register rPstack
pdropW64 :: Int.Int32 -> X64.X64 ()
pdropW64 numW64s =
    X64.add rPstack $ X64.I32 $ fromIntegral $ numW64s * 8

pdropW8 :: Int.Int32 -> X64.X64 ()
pdropW8 numW8s =
    X64.add rPstack $ X64.I32 $ fromIntegral $ numW8s

-- Parameter stack pop
ppop :: X64.Operand -> X64.X64 ()
ppop dst64@(X64.R64 _) = do
    ASM.comment $ "ppop " ++ show dst64
    X64.mov dst64 $ X64.derefOffset rPstack 0
    X64.add rPstack $ X64.I32 8
ppop dst@(X64.RL8 _) = do
    X64.mov dst $ X64.derefOffset rPstack 0
    X64.add rPstack $ X64.I32 1
ppop _ = error "ppop requires a register operand"

ppeek :: X64.Operand -> X64.X64 ()
ppeek = ppeer 0

ppeer :: Int.Int32 -> X64.Operand -> X64.X64 ()
ppeer numW64s dst =
    X64.mov dst $ X64.derefOffset rPstack (fromIntegral $ numW64s * 8)

ppeerW8 :: Int.Int32 -> X64.Operand -> X64.X64 ()
ppeerW8 numW8s dst@(X64.RL8 _)  =
    X64.mov dst $ X64.derefOffset rPstack (fromIntegral numW8s)
ppeerW8 _ _ = error "ppeerW8 requires an 8-bit register as parameter"

ptopW8 :: X64.Operand -> X64.X64 ()
ptopW8 = ppeerW8 0

ppopW8 :: X64.Operand -> X64.X64 ()
ppopW8 dst@(X64.RL8 _) = ppop dst {- x86 $ do
    mov dst $ derefOffset rPstack 0
    add rPstack $ I32 1 -}
ppopW8 _ = error "ppopW8 requires an 8-bit operand"

cpeerW8 :: Int.Int32 -> X64.Operand -> X64.X64 ()
cpeerW8 numW8s dst =
    X64.mov dst $ X64.derefOffset rCallStack (fromIntegral numW8s)

cpeer :: Int.Int32 -> X64.Operand -> X64.X64 ()
cpeer numW64s dst =
    X64.mov dst $ X64.derefOffset rCallStack (fromIntegral $ numW64s * 8)

ctop :: X64.Operand -> X64.X64 ()
ctop = cpeer 0

cpop :: X64.Operand -> X64.X64 ()
cpop dst64@(X64.R64 _) = do
    ASM.comment $ "cpop " ++ show dst64
    X64.mov dst64 $ X64.derefOffset rCallStack 0
    X64.add rCallStack $ X64.I32 8
cpop dst@(X64.RL8 _) = do
    X64.mov dst $ X64.derefOffset rCallStack 0
    X64.add rCallStack $ X64.I32 1
cpop op = error "Cpop operand unsupported: " $ show op

cdrop :: Word.Word8 -> X64.X64()
cdrop numW8s = do
    X64.add rCallStack $ X64.I32 (fromIntegral numW8s)


-- Parameter push on the parameter stack (pstack), a different
-- stack from the call stack (for which we use simply push).
-- For the parameter stack position, we reserve the RSI register.
-- Programs should avoid this register!
-- void -> 8 bytes space of any type
-- TODO: Consider the benefits of only supporting w64 on pstack and have
-- another stack for w8?
-- FIXME: ppush rPstack (a push of the parameter stack register itself) would not work!
ppush :: X64.Operand -> X64.X64 ()
ppush v | supported v = do
    ASM.comment $ "ppush " ++ show v
    -- or is it (- sz v)?
    X64.sub rPstack $ X64.I32 $ sz v
    X64.mov (X64.derefOffset rPstack 0) v
    where sz (X64.I64 _)    = 8
          sz (X64.R64 _)    = 8
          sz (X64.RR64 _ _) = 8
          sz (X64.L64 _)    = 8
          sz (X64.I32 _)    = 8
          sz (X64.I8 _)     = 1
          sz (X64.RL8 _)    = 1
          sz _              = error "Unexpected"
          supported (X64.I8 _) = True
          supported (X64.RL8 _) = True
          supported (X64.I32 _) = True
          supported (X64.R64 _) = True
          supported _ = False
ppush v = error $ "ppush doesn't support " ++ show v

cpush :: X64.Operand -> X64.X64 ()
cpush v | supported v = do
    ASM.comment $ "cpush " ++ show v
    X64.sub rCallStack $ X64.I32 $ sz v
    X64.mov (X64.derefOffset rCallStack 0) v
    where sz (X64.I64 _)    = 8
          sz (X64.R64 _)    = 8
          sz (X64.RR64 _ _) = 8
          sz (X64.L64 _)    = 8
          sz (X64.I32 _)    = 8
          sz (X64.I8 _)     = 1
          sz (X64.RL8 _)    = 1
          sz _              = error "Unexpected"
          supported (X64.I8  _) = True
          supported (X64.RL8 _) = True
          supported (X64.I32 _) = True
          supported (X64.R64 _) = True
          supported _ = False
cpush v = error $ "cpush doesn't support operand " ++ show v ++
            " (only registers are supported)"

-- Push a string on the stack
{-
ppushStr :: String -> X64.X64 ()
ppushStr s = do
    mapM_ ppush $ map (X64.I8 . X64.ascii) s
-}

{-
ppushI32 :: (Integral a) => a -> X64.X64 ()
ppushI32 n =
    ppush $ X64.I32 $ fromIntegral n
-}

ppopRestoreCallStack :: X64.X64 ()
ppopRestoreCallStack = do
    ASM.comment "Restore the previous call stack value from the p-stack"
    ppop rCallStack

-- A function made up of assembly which has a type.
-- This is used to define the built-in (basic) functions
-- Type checking could be performed on the body!
defFunBasic :: String
            -> X64.X64 ()
            -> X64.X64 ()
defFunBasic funName funBody = do
    ASM.label funName
    funBody
    ASM.label (funName ++ "_return")
    X64.ret

-- callBody = X64.callLabel

populateDictionaryKernel :: X64.X64 ()
populateDictionaryKernel = do
    ASM.comment "Populate the dictionary with some base functions from the assembly."
    ASM.comment "The dictionary is kept as a linked list rather than an array because "
    ASM.comment "there might be variable-length records in the future (e.g. the "
    ASM.comment "entry name ASCII representation)."
    ASM.comment "Simple Dictionary of cells [ prev ][ name ][ addr ]"
    ASM.comment "See also: setup"
    ASM.comment "The functions exported here can be called in the repl."
    ASM.comment "The exposed interface should allows the "
    ASM.comment "definition of new parsers (so language extension) and the emission "
    ASM.comment "of jit-code."
    ASM.comment "These are the minimal definitions that must exist in a runtime. read_w8"
    ASM.comment "and write_w8 depend on the operating system, so their implementation must"
    ASM.comment "be given in one of the OS-dependent submodules."

    dictEntry "emit_w8"
    dictEntry "emit_w64"
    dictEntry "read_w8"
    dictEntry "write_w8"

dictEntry :: String -> X64.X64 ()
dictEntry defName = do
    ASM.comment $ "ASM Dictionary entry for " ++ defName
    ASM.comment "ASM or term sequence address in memory:"
    let hash = X64.I64 $ fnv1 $ map X64.ascii defName
    ASM.comment "Backup the old rDictIdx"
    X64.mov X64.rax rDictIdx

    ASM.comment "Allocate a new record in the dictionary array"
    X64.add rDictIdx $ X64.I32 24

    ASM.comment "Set values of the new record"
    X64.mov (X64.derefOffset rDictIdx 0) X64.rax -- prev dict. entry
    X64.mov X64.rax hash
    X64.mov (X64.derefOffset rDictIdx 8) X64.rax -- hash
    X64.mov X64.rax (X64.L64 defName)
    X64.mov (X64.derefOffset rDictIdx 16) X64.rax -- code address (by label ref.)

defineEmitW8 :: X64.X64 ()
defineEmitW8 = defFunBasic "emit_w8" $ do
  ASM.comment "Takes the w8 value from the top of the stack and emits it"
  ASM.comment "in the JIT code generation area (function body definitions). Uses RAX"
  ppop X64.al
  X64.mov (X64.derefOffset rDefBodies 0) X64.al
  X64.inc rDefBodies

defineEmitW64 :: X64.X64 ()
defineEmitW64 = defFunBasic "emit_w64" $ do
  ppop X64.rax
  X64.mov (X64.derefOffset rDefBodies 0) X64.rax
  X64.add rDefBodies (X64.I32 8)

{-
writeMsgHelper msg = do
    comment $ "Write message: " ++ msg
    ppush rax
    ppush rbx
    ppush rcx
    ppush rdx
    do
        mov rax $ I64 $ fromIntegral $ linux_sys_write
        mov rbx $ I64 $ fromIntegral $ linux_stdout
        mov rdx $ I64 $ fromIntegral $ length msg
        asm $ addString msg
        mov rcx (S64 msg)
        int
    ppop rdx
    ppop rcx
    ppop rbx
    ppop rax
-}
