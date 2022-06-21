module Runtime.X64.REPL (
  defineREPL
)
where

import qualified ASM.Types            as ASM
import qualified Data.Bits            as B
import qualified Data.Int             as Int
import qualified Data.Word            as Word
import qualified Text.Printf          as Printf
import qualified X64.Types            as X64
import qualified X64.X64              as X64
import qualified Runtime.X64.BasicOps as BasicOps
import qualified Runtime.X64.Parser   as Parser

defineREPL :: X64.X64 ()
defineREPL = BasicOps.defFunBasic "repl" $ do
  ASM.comment "REPL"
  ASM.comment "Parses input from the IO and defines new terms."
  ASM.label "REPL_START"

  ASM.comment "Await some input to be available"
  X64.callLabel "read_head_w8"
  BasicOps.pdropW64 1

  ASM.comment
    "Consume any whitespace and prepare the first character"
  Parser.callOptionalParser
    "parse_wss" "REPL_ERR_UNKNOWN_INPUT"

  ASM.comment
    "Read a term using parse_identifier:"
  -- TODO: Rename 'identifier' into 'term' in the parser!
  Parser.callRequiredParser
    "parse_identifier" "REPL_ERR_UNKNOWN_INPUT"

  ASM.comment "Hash the term we've read:"

  X64.callLabel "term_hash"

  -- The first word is the operation we wish to make.
  -- There are two operations for now, define and call.
  ASM.comment
    "Switch to a different functionality"
  ASM.comment
    "depending on the hash of the term we've just read:"

  BasicOps.ppop X64.rax
  X64.mov X64.rbx (X64.I64 $ BasicOps.fnv1s "def")
  X64.cmp X64.rax X64.rbx
  X64.jeNear "REPL_DEF"
  X64.mov X64.rbx (X64.I64 $ BasicOps.fnv1s "run")
  X64.cmp X64.rax X64.rbx
  X64.jeNear "REPL_RUN"
  X64.mov X64.rbx (X64.I64 $ BasicOps.fnv1s "q")
  X64.cmp X64.rax X64.rbx
  X64.jeNear "REPL_QUIT"

  ASM.label "REPL_ERR_UNKNOWN_INPUT"
  -- writeMsgHelper "Unknown command! (expected: def/run/q)\n"
  X64.jmpLabel "REPL_START"

  -------------------------
  -------------------------
  ASM.label "REPL_DEF"

  ASM.comment "Consume any whitespace"
  Parser.callOptionalParser "parse_wss" "REPL_ERR_UNKNOWN_INPUT"
  Parser.callRequiredParser "parse_def" "REPL_ERR_FAILED_DEF"

  -- writeMsgHelper "OK, defined.\n"

  ASM.comment "After the definition resume from the beginning."
  X64.jmpLabel "REPL_START"

  -------------------------
  -------------------------
  ASM.label "REPL_RUN"

  ASM.comment "Consume any whitespace"
  Parser.callOptionalParser "parse_wss" "REPL_ERR_UNKNOWN_INPUT"
  Parser.callRequiredParser "parse_identifier" "REPL_ERR_NOT_A_TERM"

  X64.callLabel "term_hash_look"
  -- Check for an unknown term (do nothing in that case).
  BasicOps.ppop X64.rax
  X64.cmp X64.rax (X64.I32 0)
  X64.jeNear "REPL_RUN_UNDEFINED"

  ASM.comment
    "Take the '.addr' field from the term found by term_hash_look"
  BasicOps.ppop X64.rax
  X64.mov X64.rax (X64.derefOffset X64.rax 16)

  ASM.comment "Evaluate the found entry"
  X64.call X64.rax

  -- writeMsgHelper "Run term done.\n"
  X64.jmpLabel "REPL_START"

  ASM.label "REPL_ERR_NOT_A_TERM"
  -- writeMsgHelper "Not a valid term to run!\n"
  X64.jmpLabel "REPL_START"

  ASM.label "REPL_ERR_FAILED_DEF"
  -- writeMsgHelper "Failed to parse definition!\n"
  BasicOps.ppushContCharW8
  -- writeMsgHelper "Unable to handle char: '"
  X64.callLabel "dbg_dump_ptop_w8"
  -- writeMsgHelper "'\n"
  BasicOps.ppopW8 X64.al
  X64.jmpLabel "REPL_START"

  ASM.label "REPL_RUN_UNDEFINED"
  -- assertPtop 0 "TERM_LOOK failed so the result should be 0."
  BasicOps.pdropW64 1 -- Drop the 0 From TERM_LOOK
  -- writeMsgHelper "L.1158 Undefined term!\n"
  X64.jmpLabel "REPL_START"

  ASM.label "REPL_QUIT"
