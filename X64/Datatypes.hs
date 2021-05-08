{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module X64.Datatypes where

import Data.Word
import qualified Data.Sequence as S
import ASM.Datatypes
import ASM.ASM
import Control.Applicative
import Control.Monad (ap, liftM)
import Control.Monad.Trans.Writer

-- X64 monad

newtype X64 a = X64 { runX64 :: ASM Word64 a }
    deriving (Monad)

runASM :: ASM Word64 a -> X64 a
runASM act = X64 (act)

instance Functor X64 where
    fmap = liftM

instance Applicative X64 where
    pure   = return
    (<*>)  = ap

instance Commentable X64 where
    comment = runASM . comment

instance Labelable X64 where
    label = runASM . label

-- X64 Opcode monad that generates a single opcode

type Opcode = WriterT (S.Seq (ASMBytes Word64)) X64

{-
newtype Opcode a = Opcode { getOpcode :: X64 a } 
    deriving (Monad)
-}

-- X64 operands, used for parameter passing to opcode combinators

data Operand -- Corresponds to addressing methods
  = I64   Word64  -- 64bit Immediate Integer
  | I32   Word32  -- 32bit Immediate Integer
  | I8    Word8   -- 8bit  Immediate Integer
  | R64   Reg     -- Register, 64 bits
  | RL8   Reg    -- Register, 8 least significative bits (LOW)
  | A     Word64  -- Memory Address (virtual)
  | RR64  -- Refer to memory at "address from register plus offset"
          Reg     -- Register
          Integer -- Offset
{-| RR8   -- Refer to memory at "addres from register plus offset"
          Reg64   -- Register
          Integer -- Offset -}
  | L64   String  -- Label Memory Address (virtual)
--  | S64   String  -- Reference to the strings table
  deriving (Eq)

instance Show Operand where
    show (I64 w64)  = show w64
    show (I32 w32)  = show w32
    show (I8  w8)   = show w8
    show (R64 r)    = show r
    show (RL8 r)    = show r
    show (RR64 r o) = "[" ++ show r ++ "+" ++ show o ++ "]"
    show (L64 s)    = s
--    show (S64 s)    = s

data Reg 
  = AX  -- Accumulator
  | CX  -- Counter (Loop counters)
  | DX  -- Data
  | BX  -- Base / General Purpose
  | SP  -- Current stack pointer
  | BP  -- Previous Stack Frame Link
  | SI  -- Source Index Pointer (for string operations)
  | DI  -- Destination Index Pointer (for string operations)
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Eq, Show)

{-
data Reg64
  = RAX  -- Accumulator
  | RCX  -- Counter (Loop counters)
  | RDX  -- Data
  | RBX  -- Base / General Purpose
  | RSP  -- Current stack pointer
  | RBP  -- Previous Stack Frame Link
  | RSI  -- Source Index Pointer (for string operations)
  | RDI  -- Destination Index Pointer (for string operations)
  | RR8  -- R8
  | RR9  -- R9
  | RR10 -- R10
  | RR11 -- R11
  | RR12 -- R12
  | RR13 -- R13
  | RR14 -- R14
  | RR15 -- R15
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
  deriving (Eq, Show)

rax = R64 RAX
rbx = R64 RBX
rsp = R64 RSP
rcx = R64 RCX
rdx = R64 RDX
rbp = R64 RBP
rdi = R64 RDI
rsi = R64 RSI
r8  = R64 R8
r9  = R64 R9
r10 = R64 R10
r11 = R64 R11
r12 = R64 R12
r13 = R64 R13
r14 = R64 R14
r15 = R64 R15

al  = R8 AL
cl  = R8 CL
dl  = R8 DL
bl  = R8 BL

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
  RAX  -> 0 -- 000
  RCX  -> 1 -- 001
  RDX  -> 2 -- 010
  RBX  -> 3 -- 011
  RSP  -> 4 -- 100
  RBP  -> 5 -- 101
  RSI  -> 6 -- 110
  RDI  -> 7 -- 111
  RR8  -> 0 -- Superior register access is done using
  RR9  -> 1 -- one of the additional REX bits...
  RR10 -> 2
  RR11 -> 3
  RR12 -> 4
  RR13 -> 5
  RR14 -> 6
  RR15 -> 7
-}

rax = R64 AX
rbx = R64 BX
rsp = R64 SP
rcx = R64 CX
rdx = R64 DX
rbp = R64 BP
rdi = R64 DI
rsi = R64 SI
r8  = R64 R8
r9  = R64 R9
r10 = R64 R10
r11 = R64 R11
r12 = R64 R12
r13 = R64 R13
r14 = R64 R14
r15 = R64 R15

al  = RL8 AX
cl  = RL8 CX
dl  = RL8 DX
bl  = RL8 BX

index :: Reg -> Word8
index x = case x of
  AX  -> 0 -- 000
  CX  -> 1 -- 001
  DX  -> 2 -- 010
  BX  -> 3 -- 011
  SP  -> 4 -- 100
  BP  -> 5 -- 101
  SI  -> 6 -- 110
  DI  -> 7 -- 111
  R8  -> 0 -- Superior register access is done using
  R9  -> 1 -- one of the additional REX bits...
  R10 -> 2
  R11 -> 3
  R12 -> 4
  R13 -> 5
  R14 -> 6
  R15 -> 7

-- Indicate whether it's one of the scratch registers that are added to 
-- the 64 bit architecture
isSupReg :: Reg -> Bool
isSupReg R8  = True
isSupReg R9  = True
isSupReg R10 = True
isSupReg R11 = True
isSupReg R12 = True
isSupReg R13 = True
isSupReg R14 = True
isSupReg R15 = True
isSupReg _   = False

