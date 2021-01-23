{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}

module ASM.Datatypes where

import Data.ByteString.Lazy
import Data.Sequence as S
import Data.Map as M

import Control.Monad.Except
import Control.Monad.Trans.State

import Data.Word

data ASMState t_addr = ASMState
    { file_addr    :: t_addr -- Current offset in generated image file
    , rva          :: t_addr -- Current offset from the image base address 
                   -- in the memory-loaded data.
                   -- This is needed because some output files 
                   -- (e.g. portable executable, require to 
                   -- tell the Windows loader where values will be 
                   -- stored in memory and those also need to be aligned.
                   -- Hence the need for offsets different from the 
                   -- file point of view (bytes emitted so far).
                   -- This is initially 0. It's often refered to as
                   -- RVA (Relative Virtual Address) in Microsoft
                   -- documentation.
    , image_base   :: t_addr -- The in-memory image base location
    , contents     :: S.Seq (ASMItem t_addr)
                            -- Sequence of opcodes, labels and comments
    , labels       :: M.Map String (t_addr, t_addr) -- value fst: file address; 
                                                    -- value snd: rva
    , uid          :: Integer -- unique label name generator counter
    , zero         :: t_addr -- Dummy value, always 0 to have access to the t_addr type (Hack)
    , resolved_asm :: ByteString -- Filled in the final step when all assembly is done
    }

type ASM t_addr = StateT (ASMState t_addr) (Except String)

data ASMItem t_addr =
        -- In general the first t_addr is the image file address and the 
        -- second t_addr is the rva
        ASMOpcode  t_addr t_addr (S.Seq (ASMBytes t_addr)) String -- nasm equiv.
    |   ASMComment String
    |   ASMLabel   t_addr t_addr String
    |   ASMBytes   t_addr t_addr (S.Seq (ASMBytes t_addr))

data Size       = Bytes1 | Bytes2 | Bytes4 | Bytes8
data Endianness = LE | BE
data Ref        = VA  -- Virtual Address (in-memory address)
                | RVA -- Relative Virtual Address (in-memory address minus 
                      -- image base address) 
                | IA  -- Image Address (in-file address, offset from the 
                      -- beginning of the file)

data ASMBytes t_addr = 
        -- Literal bytes:
        BytesLiteral ByteString

        -- Label address reference:
    |   BytesLabelRef Ref Size Endianness String

        -- The signed offset to a label from just after the reference:
        -- This is used in relative jump instructions
    |   BytesLabelRefOffset t_addr t_addr Ref Size Endianness String

        -- The unsigned offset from the first label to the second one:
        -- the first label must be <= than the second one and the delta
        -- must fit the given Size (this is to be error-checked at 
        -- run time):
        -- It's used when defining the executable file headers.
    |   BytesLabelDiff Ref Size Endianness String {- <= -} String

class Commentable m where
    comment :: String -> m ()

class Labelable m where
    label :: String -> m ()
