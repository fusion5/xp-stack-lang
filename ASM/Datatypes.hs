module ASM.Datatypes where

import Data.Word
import Data.Map as M
import Data.Sequence as S
import Data.Foldable as F

import Control.Monad.Except
import Control.Monad.Trans.State

-- A construct that emits assembly bytes with label references of type lt
data ASMEmit lt =
      SWord8 Word8    -- A literal 8 bit char
    | SProgLabel64 lt -- The 64 bit offset to a label from the program start
    | SProgLabel32 lt -- The 32 bit offset to a label from the program start
    | SRelOffsetToLabel32 -- A 32 bit offset to a label from just AFTER the ref.
        Word64 -- Convenience copy of the current offset
        lt     -- Target label
    | SRelOffsetToLabel8 -- Same as SRelOffsetToLabel32 but 8 bits only.
        Word64 -- Convenience copy of the current offset
        lt     -- Target label
    | SProgSize64 -- Program size, 64 bits
    | SStrRef64   -- Reference to a string from the strings table
        String
    deriving (Eq, Show)

-- Assembly with labels and documentation
data ASMCode =
      ASMEmit   Word64           -- Offset convenience copy
                [ASMEmit String] -- A sequence of bytes to emit
    | ASMLabel  Word64           -- Offset convenience copy
                String           -- A label
    | ASMDoc    Word64           -- Offset convenience copy
                String           -- Documentation
    deriving (Eq, Show)

emitHasLabel (SProgLabel64 l)          = Just l
emitHasLabel (SProgLabel32 l)          = Just l
emitHasLabel (SRelOffsetToLabel32 _ l) = Just l
emitHasLabel (SRelOffsetToLabel8 _ l)  = Just l
emitHasLabel _                         = Nothing

{-
lenBytesCode :: ASMCode -> Integer
lenBytesCode (ASMEmit _ ws) = sum $ Prelude.map lenBytes ws
lenBytesCode _ = 0
-}

lenBytes :: ASMEmit String -> Integer
lenBytes (SWord8 _) = 1 -- A single byte
lenBytes (SProgLabel64 _) = 8
lenBytes (SProgLabel32 _) = 4
lenBytes (SRelOffsetToLabel32 _ _) = 4
lenBytes (SRelOffsetToLabel8  _ _) = 1
lenBytes (SProgSize64) = 8
lenBytes (SStrRef64 _) = 8

type ASM a = StateT ASMState (Except String) a

data ASMState = ASMState
    { asm_instr    :: S.Seq ASMCode -- Sequence of opcodes, labels and docs
--  , asm_bytes    :: S.Seq Word8   -- Resulting assembly code
    , asm_offset   :: Word64  -- Current offset
    , asm_uid      :: Integer -- Counter used to generate unique label names
    , asm_strs     :: M.Map String Word64 -- String addresses
    , asm_lbls     :: M.Map String Word64 -- Labeled addresses
    , asm_ebuf     :: S.Seq (ASMEmit String) -- Opcode emit Buffer 
    , asm_ebuf_off :: Word64
--    , asm_tyenv  :: M.Map String Type -- Type environment
    } deriving (Eq, Show)

-- Initial assembler state.
istate :: Word64
       -> ASMState
istate start = ASMState
    { asm_instr  = S.empty
--  , asm_bytes  = S.empty
    , asm_offset = start
    , asm_lbls   = M.empty
    , asm_uid    = 0
    , asm_strs = M.empty
    , asm_ebuf = S.empty
    , asm_ebuf_off = 0 -- The offset at which buffering starts.
--    , _typeEnv = M.empty
    }

-- bytes :: ASMState -> [Word8]
-- bytes = F.toList . asm_bytes
