{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module X86.Datatypes where

import Data.Word
import ASM.Datatypes
import Control.Applicative
import Control.Monad (ap, liftM)

-- X86_64 monad

newtype X86_64 a = X86_64 { runX86 :: ASM a }
    deriving (Monad)

asm :: ASM () -> X86_64 ()
asm act = X86_64 (act)

instance Functor X86_64 where
    fmap = liftM

instance Applicative X86_64 where
    pure   = return
    (<*>)  = ap

{-
instance Monad X86_64 where
    (>>=) (X86_64 asmst) (next) = next asmst
    return                      = pure
    -- fail msg       = X86 (Left msg)
-}
-- Assembly operands

data Val         -- Corresponds to addressing methods
  = I64   Word64  -- 64bit Immediate Integer
  | I32   Word32  -- 32bit Immediate Integer
  | I8    Word8   -- 8bit  Immediate Integer
  | R64   Reg64   -- Register
  | R8    Reg8    -- Register, 8 bits
  | A     Word64  -- Addr
  | RR64          -- Refer to memory at "address from register plus offset".
          Reg64   -- Register
          Integer -- Offset
  | L64   String  -- Label
  | S64   String  -- Reference to the strings table
  deriving (Eq)

instance Show Val where
    show (I64 w64)  = show w64
    show (I32 w32)  = show w32
    show (I8  w8)   = show w8
    show (R64 r)    = show r
    show (R8 r)     = show r
    show (RR64 r o) = "[" ++ show r ++ "+" ++ show o ++ "]"
    show (L64 s)    = s
    show (S64 s)    = s

data Reg64
  = RAX  -- Accumulator
  | RCX  -- Counter (Loop counters)
  | RDX  -- Data
  | RBX  -- Base / General Purpose
  | RSI  -- Source Index Pointer (for string operations)
  | RDI  -- Destination Index Pointer (for string operations)
  | RSP  -- Current stack pointer
  | RBP  -- Previous Stack Frame Link
  | RR8  -- R8
  | RR9  -- R9
  | R10  -- R10
  | R11  -- R11
  | R12  -- R12
  | R13  -- R13
  | R14  -- R14
  | R15  -- R15
  deriving (Eq, Show)

data Reg8 
  = AL
  | CL
  | DL
  | BL
  | SIL
  | DIL
  | SPL
  | BPL
  | R8B
  | R9B
  | R10B
  | R11B
  | R12B
  | R13B
  | R14B
  | R15B
  | R16B
  deriving (Eq, Show)

rax = R64 RAX
rbx = R64 RBX
rsp = R64 RSP
rcx = R64 RCX
rdx = R64 RDX
rbp = R64 RBP
rdi = R64 RDI
rsi = R64 RSI

al = R8 AL
cl = R8 CL
dl = R8 DL
bl = R8 BL

index8 :: Reg8 -> Word8
index8 x = case x of
  AL   -> 0
  CL   -> 1
  DL   -> 2
  BL   -> 3
  SIL  -> 4
  DIL  -> 5
  SPL  -> 6
  BPL  -> 7
  R8B  -> 8
  R9B  -> 9
  R10B -> 10
  R11B -> 11
  R12B -> 12
  R13B -> 13
  R14B -> 14
  R15B -> 15

index :: Reg64 -> Word8
index x = case x of
  RAX -> 0 -- 000
  RCX -> 1 -- 001
  RDX -> 2 -- 010
  RBX -> 3 -- 011
  RSP -> 4 -- 100
  RBP -> 5 -- 101
  RSI -> 6 -- 110
  RDI -> 7 -- 111
{-  R8  -> 8 Not quite right... superior register access is done using
  R9  -> 9   a REX bit...
  R10 -> 10
  R11 -> 11
  R12 -> 12
  R13 -> 13
  R14 -> 14
  R15 -> 15 -}

