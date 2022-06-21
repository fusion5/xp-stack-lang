module Runtime.X64.Debug where

import qualified ASM.Types            as ASM
import qualified X64.Types            as X64
import qualified X64.X64              as X64
import qualified Runtime.X64.BasicOps as BasicOps
import qualified Control.Monad        as Monad

-- X86 debug utilities for all operating systems

doc :: String -> X64.X64 ()
doc = ASM.comment

-- It's here for tests, I think the caller should ensure alignment so it
-- should be removed...
withStackAlign :: X64.X64() -> X64.X64 ()
withStackAlign act = do
  doc "Test 16-byte call stack alignment"
  X64.sub BasicOps.rCallStack (X64.I32 0x08)
  act
  X64.add BasicOps.rCallStack (X64.I32 0x08)

-- Type :w8  -> :w8
-- Func :val -> :val
-- Does nothing; as a side effect, it prints the top of the stack on stdout.
defineDbgDumpPtopW8 :: X64.X64 ()
defineDbgDumpPtopW8 = BasicOps.defFunBasic "dbg_dump_ptop_w8" $
  BasicOps.withRegisters [X64.rax, X64.rbx, X64.rcx, X64.rdx] $ do
    doc "Clear rax and place the value we wish to write into it:"
    X64.xor X64.rax X64.rax
    BasicOps.ptopW8 X64.al

    doc "An 8 bit value needs 2 chars to be printed. RCX is the counter."
    X64.mov X64.rcx $ X64.I64 2

    ASM.label "DBG_DUMP_PTOP_8_START"
    X64.mov X64.rbx $ X64.I64 0x10
    X64.xor X64.rdx X64.rdx
    X64.cmp X64.rcx $ X64.I32 0
    X64.je "DBG_DUMP_PTOP_8_END"
    X64.div_ X64.rbx
    doc "Convert dl, which holds the remainder, to ascii, then push it."

    X64.cmp X64.rdx $ X64.I32 10
    X64.jl "DBG_DUMP_PTOP_8_LT10"
    doc "Value between [10 and 15]"
    doc "Add 55 to rdx"
    X64.add X64.rdx $ X64.I32 55
    BasicOps.ppush X64.dl
    X64.dec X64.rcx
    X64.jmpLabel "DBG_DUMP_PTOP_8_START"

    ASM.label "DBG_DUMP_PTOP_8_LT10"
    doc "Value between [0 and 9]"
    doc "Add 0x30 to rdx"
    X64.add X64.rdx $ X64.I32 0x30
    BasicOps.ppush X64.dl
    X64.dec X64.rcx
    X64.jmpLabel "DBG_DUMP_PTOP_8_START"

    ASM.label "DBG_DUMP_PTOP_8_END"
    doc "2 times write char. Conveniently, using the stack before "
    doc "printing reverses the "
    doc "order of the chars (which we need to do)."

    doc "*** OUTPUT"

    Monad.replicateM_ 2 $ do
      X64.callLabel "write_w8"
      BasicOps.pdropW64 1

    doc "Write a newline"
    BasicOps.ppush $ X64.I8 0x0A
    X64.callLabel "write_w8"
    BasicOps.pdropW64 1

  -- doc "Test stack alignment padding"
  -- add rCallStack (I32 0x08)


-- Type :w64 -> :w64
-- Func :val -> :val
-- Does nothing; as a side effect, it prints the top of the stack on stdout.
defineDbgDumpPtopW64 :: X64.X64 ()
defineDbgDumpPtopW64 = BasicOps.defFunBasic "dbg_dump_ptop_w64" $
  BasicOps.withRegisters [X64.rax, X64.rbx, X64.rcx, X64.rdx] $ do

    doc "Place the value we wish to write into rax:"
    BasicOps.ppeek X64.rax

    doc "A 64 bit value needs 16 chars to be printed. RCX is the counter."
    X64.mov X64.rcx $ X64.I64 16

    ASM.label "DBG_DUMP_PTOP_64_START"
    X64.mov X64.rbx $ X64.I64 0x10
    X64.xor X64.rdx X64.rdx
    X64.cmp X64.rcx $ X64.I32 0
    X64.je "DBG_DUMP_PTOP_64_END"
    X64.div_ X64.rbx
    doc "Print rdx, which holds the remainder."

    X64.cmp X64.rdx $ X64.I32 10
    X64.jl "DBG_DUMP_PTOP_64_LT10"
    doc "Value between [10 and 15]"
    doc "Add 55 to rdx to convert to A..F"
    X64.add X64.rdx $ X64.I32 55
    BasicOps.ppush X64.dl
    X64.dec X64.rcx
    X64.jmpLabel "DBG_DUMP_PTOP_64_START"

    ASM.label "DBG_DUMP_PTOP_64_LT10"
    doc "Value between [0 and 9]"
    doc "Add 0x30 to rdx"
    X64.add X64.rdx $ X64.I32 0x30
    BasicOps.ppush X64.dl
    X64.dec X64.rcx
    X64.jmpLabel "DBG_DUMP_PTOP_64_START"

    ASM.label "DBG_DUMP_PTOP_64_END"

    doc "*** OUTPUT"

    doc "16 times write char. Conveniently, using the stack before "
    doc "printing reverses the "
    doc "order of the chars (which we need to do)."
    Monad.replicateM_ 16 $ do
      X64.callLabel "write_w8"
      BasicOps.pdropW64 1

    doc "Write a newline char"
    BasicOps.ppush $ X64.I8 0x0A
    X64.callLabel "write_w8"
    BasicOps.pdropW64 1

defineDbgDumpDictionary :: X64.X64 ()
defineDbgDumpDictionary = BasicOps.defFunBasic "dbg_dump_dictionary" $
  BasicOps.withRegisters [X64.rbx, X64.rcx] $ do
    doc "Dump dictionary"
    -- writeMsgHelper "Dictionary\n"
    -- writeMsgHelper "----------\n"
    X64.mov X64.rbx X64.r11
    ASM.label "DBG_DUMP_DICTIONARY_LOOP"
    do
      X64.cmp X64.rbx $ X64.I32 0
      X64.jeNear "DBG_DUMP_DICTIONARY_END"

      X64.mov X64.rcx (X64.derefOffset X64.rbx 8)
      BasicOps.ppush X64.rcx
      X64.callLabel "dbg_dump_ptop_w64"
      BasicOps.pdropW64 1

      doc "Advance the dictionary to the following item (def.prev)"
      X64.mov X64.rbx (X64.derefOffset X64.rbx 0)

      X64.jmpLabel "DBG_DUMP_DICTIONARY_LOOP"
    ASM.label "DBG_DUMP_DICTIONARY_END"

