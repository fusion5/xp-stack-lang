module Lang.BasicFunctions where

import X64.Datatypes
import X64.X64

import ASM.ASM
import ASM.Datatypes

import Text.Printf

import Data.Int
import Data.Word
import qualified Data.Bits as B
import Control.Monad

import Lang.Linux

rPstack    = r13 -- rsi
rCallStack = rsp
rDefBodies = r15
rDictIdx   = r14

-- String hash parameters
fnvOffsetBasis = 0xCBF29CE484222325
fnvPrime       = 0x100000001B3

fnvFold :: Bool -> Word8 -> Word64 -> Word64
fnvFold False x h = (fnvPrime * h) `B.xor` fromIntegral x
fnvFold True  x h = fnvPrime * (h  `B.xor` fromIntegral x)

-- A helper function to compute a fnv-1 hash
fnv1 :: [Word8] -> Word64
fnv1 = foldr (fnvFold False) fnvOffsetBasis

fnv1s :: String -> Word64
fnv1s = fnv1 . map ascii

fnv1Integral = fromIntegral . fnv1s

fnv1Hex s = printf "%08X\n" $ fnv1s s

-- Parameter stack drop, alters the parameter stack register rPstack
pdrop :: Int32 -> X64 ()
pdrop numW64s =
    add rPstack $ I32 $ fromIntegral $ numW64s * 8

pdropW8 :: Int32 -> X64 ()
pdropW8 numW8s = 
    add rPstack $ I32 $ fromIntegral $ numW8s

-- Parameter stack pop
ppop :: Operand -> X64 ()
ppop dst64@(R64 _) = do
    comment $ "ppop " ++ show dst64
    mov dst64 $ derefOffset rPstack 0
    add rPstack $ I32 8
ppop dst@(RL8 _) = do
    mov dst $ derefOffset rPstack 0
    add rPstack $ I32 1

ppeek = ppeer 0

ppeer :: Int32 -> Operand -> X64 ()
ppeer numW64s dst =
    mov dst $ derefOffset rPstack (fromIntegral $ numW64s * 8)

ppeerW8 :: Int32 -> Operand -> X64 ()
ppeerW8 numW8s dst@(RL8 r)  =
    mov dst $ derefOffset rPstack (fromIntegral numW8s)
ppeerW8 _ _ = error "ppeerW8 requires an 8-bit register as parameter"

ptopW8 :: Operand -> X64 ()
ptopW8 = ppeerW8 0

ppopW8 :: Operand -> X64 ()
ppopW8 dst@(RL8 reg) = ppop dst {- x86 $ do
    mov dst $ derefOffset rPstack 0
    add rPstack $ I32 1 -}

cpeerW8 :: Int32 -> Operand -> X64 ()
cpeerW8 numW8s dst =
    mov dst $ derefOffset rCallStack (fromIntegral numW8s)

cpeer :: Int32 -> Operand -> X64 ()
cpeer numW64s dst = 
    mov dst $ derefOffset rCallStack (fromIntegral $ numW64s * 8)

ctop :: Operand -> X64 ()
ctop = cpeer 0

cpop :: Operand -> X64 ()
cpop dst64@(R64 _) = do
    comment $ "cpop " ++ show dst64
    mov dst64 $ derefOffset rCallStack 0
    add rCallStack $ I32 8
cpop dst@(RL8 _) = do
    mov dst $ derefOffset rCallStack 0
    add rCallStack $ I32 1

cdrop :: Word8 -> X64()
cdrop numW8s = do
    add rCallStack $ I32 (fromIntegral numW8s)


-- Parameter push on the parameter stack (pstack), a different 
-- stack from the call stack (for which we use simply push).
-- For the parameter stack position, we reserve the RSI register.
-- Programs should avoid this register!
-- void -> 8 bytes space of any type
-- TODO: Consider the benefits of only supporting w64 on pstack and have 
-- another stack for w8?
-- FIXME: ppush rPstack (a push of the parameter stack register itself) would not work!
ppush :: Operand -> X64 ()
ppush v | supported v = do
    comment $ "ppush " ++ show v
    -- or is it (- sz v)?
    sub rPstack $ I32 $ sz v
    mov (derefOffset rPstack 0) v
    where sz (I64 _)    = 8
          sz (R64 _)    = 8
          sz (RR64 _ _) = 8
          sz (L64 _)    = 8
          sz (I32 _)    = 8
          sz (I8 _)     = 1
          sz (RL8 _)    = 1
          supported (I8 _) = True
          supported (RL8 _) = True
          supported (I32 _) = True
          supported (R64 _) = True
          supported _ = False
ppush v = error $ "ppush doesn't support " ++ show v

cpush :: Operand -> X64 ()
cpush v | supported v = do
    comment $ "cpush " ++ show v
    sub rCallStack $ I32 $ sz v
    mov (derefOffset rCallStack 0) v
    where sz (I64 _)    = 8
          sz (R64 _)    = 8
          sz (RR64 _ _) = 8
          sz (L64 _)    = 8
          sz (I32 _)    = 8
          sz (I8 _)     = 1
          sz (RL8 _)    = 1
          supported (I8  _) = True
          supported (RL8 _) = True
          supported (I32 _) = True
          supported (R64 _) = True
          supported _ = False
cpush v = error $ "cpush doesn't support operand " ++ show v ++ 
            " (only registers are supported)"

-- Push a string on the stack
ppushStr :: String -> X64 ()
ppushStr s = do 
    mapM ppush $ map (I8 . ascii) s
    return ()

ppushI32 :: (Integral a) => a -> X64 ()
ppushI32 n = do
    ppush $ I32 $ fromIntegral n

ppushAlignCallStack16bytes :: X64 ()
ppushAlignCallStack16bytes = do
    comment "Backup the current call stack pointer to the p-stack"
    ppush rCallStack

    -- Perform the division by 16 bytes, i.e. 16*8 = 128 = 0x80
    comment "This subtracts from the stack pointer until it's divisible by 128."
    comment "The result is a 16-byte aligned rPstack."
    comment "AND(rPstack, imm32 sign-extended to 64-bits):"
    -- and_ rCallStack (I32 0xFFFFFF7F)
    and_ rCallStack (I32 0xFFFFFF80)

ppopRestoreCallStack :: X64 ()
ppopRestoreCallStack = do
    comment "Restore the previous call stack value from the p-stack"
    ppop rCallStack
 
cpush_x64_32ShadowBytes :: X64 ()
cpush_x64_32ShadowBytes = do
    sub rCallStack (I32 0x20) 

cpop_x64_32ShadowBytes :: X64 ()
cpop_x64_32ShadowBytes = do
    add rCallStack (I32 0x20) 

-- A function made up of assembly which has a type.
-- This is used to define the built-in (basic) functions
-- No type checking is performed on the body!
defFunBasic :: String
            -> X64 ()
            -> X64 ()
defFunBasic funName funBody = do
    -- envAddType funName funTy
    label funName
    funBody
    label (funName ++ "_return")
    ret

callBody = callLabel 

   

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
