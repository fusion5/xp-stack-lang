module Lang.BasicFunctions where

import X86.Datatypes
import X86.X86

import ASM.ASM
import ASM.Datatypes

import Lang.Datatypes

import Text.Printf

import Data.Int
import Data.Word
import qualified Data.Bits as B
import Control.Monad

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

-- Parameter stack drop, alters the parameter stack register rsi
pdrop :: Int32 -> Lang ()
pdrop numW64s =
    x86 $ add rsi $ I32 $ fromIntegral $ numW64s * 8

pdropW8 :: Int32 -> Lang ()
pdropW8 numW8s = 
    x86 $ add rsi $ I32 $ fromIntegral $ numW8s

-- Parameter stack pop
ppop :: Val -> Lang ()
ppop dst64@(R64 _) = do
    doc $ "ppop " ++ show dst64
    x86 $ mov dst64 $ derefOffset rsi 0
    x86 $ add rsi $ I32 8
ppop dst@(R8 _) = x86 $ do
    mov dst $ derefOffset rsi 0
    add rsi $ I32 1

ppeek = ppeer 0

ppeer :: Int32 -> Val -> Lang ()
ppeer numW64s dst =
    x86 $ mov dst $ derefOffset rsi (fromIntegral $ numW64s * 8)

ppeerW8 :: Int32 -> Val -> Lang ()
ppeerW8 numW8s dst@(R8 r)  =
    x86 $ mov dst $ derefOffset rsi (fromIntegral numW8s)
ppeerW8 _ _ = error "ppeerW8 requires an 8-bit register as parameter"

ptopW8 :: Val -> Lang ()
ptopW8 = ppeerW8 0

ppopW8 :: Val -> Lang ()
ppopW8 dst@(R8 reg) = ppop dst {- x86 $ do
    mov dst $ derefOffset rsi 0
    add rsi $ I32 1 -}

cpeerW8 :: Int32 -> Val -> X86_64 ()
cpeerW8 numW8s dst =
    mov dst $ derefOffset rsp (fromIntegral numW8s)

cpeer :: Int32 -> Val -> X86_64 ()
cpeer numW64s dst = 
    mov dst $ derefOffset rsp (fromIntegral $ numW64s * 8)

ctop :: Val -> X86_64 ()
ctop = cpeer 0

-- Parameter push on the parameter stack (pstack), a different 
-- stack from the call stack (for which we use simply push).
-- For the parameter stack position, we reserve the RSI register.
-- Programs should avoid this register!
-- void -> 8 bytes space of any type
-- TODO: Consider the benefits of only supporting w64 on pstack and have 
-- another stack for w8?
-- FIXME: Rather than RSI use RBP which isn't used anyway.
ppush :: Val -> Lang ()
ppush v | supported v = do
    doc $ "ppush " ++ show v
    x86 $ sub rsi $ I32 $ sz v
    x86 $ mov (derefOffset rsi 0) v
    where sz (I64 _)    = 8
          sz (R64 _)    = 8
          sz (RR64 _ _) = 8
          sz (L64 _)    = 8
          sz (I32 _)    = 8
          sz (I8 _)     = 1
          sz (R8 _)     = 1
          supported (I8 _) = True
          supported (R8 _) = True
          supported (I32 _) = True
          supported (R64 _) = True
          supported _ = False
ppush v = error $ "ppush doesn't support " ++ show v

-- Scratch stack push
spush :: Val -> Lang ()
spush v | supported v = do
    doc $ "spush " ++ show v
    x86 $ sub rdi $ I32 $ sz v
    x86 $ mov (derefOffset rdi 0) v
    where sz (I64 _)    = 8
          sz (R64 _)    = 8
          sz (RR64 _ _) = 8
          sz (L64 _)    = 8
          sz (I32 _)    = 8
          sz (I8 _)     = 1
          sz (R8 _)     = 1
          supported (I8 _)  = True
          supported (R8 _)  = True
          supported (I32 _) = True
          supported (R64 _) = True
          supported _       = False
spush v = error $ "spush doesn't support " ++ show v

spop :: Val -> Lang ()
spop dst64@(R64 _) = do
    doc $ "spop " ++ show dst64
    x86 $ mov dst64 $ derefOffset rdi 0
    x86 $ add rdi $ I32 8
spop dst@(R8 _) = x86 $ do
    doc $ "spop " ++ show dst
    -- TODO: Might be a bug for dst = dl
    mov dst $ derefOffset rdi 0
    add rdi $ I32 1

cpush :: Val -> Lang ()
cpush v | supported v = do
    doc $ "cpush " ++ show v
    x86 $ sub rsp $ I32 $ sz v
    x86 $ mov (derefOffset rsp 0) v
    where sz (I64 _)    = 8
          sz (R64 _)    = 8
          sz (RR64 _ _) = 8
          sz (L64 _)    = 8
          sz (I32 _)    = 8
          sz (I8 _)     = 1
          sz (R8 _)     = 1
          supported (I8 _) = True
          supported (R8 _) = True
          supported (I32 _) = True
          supported (R64 _) = True
          supported _ = False
cpush v = error $ "cpush doesn't support " ++ show v

-- Push a string on the stack
ppushStr :: String -> Lang ()
ppushStr s = do 
    mapM ppush $ map (I8 . ascii) s
    return ()

ppushI32 :: (Integral a) => a -> Lang ()
ppushI32 n = do
    ppush $ I32 $ fromIntegral n

-- A function made up of assembly which has a type.
-- This is used to define the built-in (basic) functions
-- No type checking is performed on the body!
defFunBasic :: String
            -> Lang ()
            -> Lang ()
defFunBasic funName funBody = do
    -- envAddType funName funTy
    x86 $ asm $ setLabel funName
    funBody
    x86 $ asm $ setLabel (funName ++ "_return")
    x86 $ ret

callBody :: String -> Lang () 
callBody l = do
    x86 $ callLabel l
