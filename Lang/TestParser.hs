module Lang.TestParser where 

import Control.Monad.Trans.State
import Control.Monad.Except

import X86.Datatypes
import X86.X86
import ASM.ASM
import ASM.Datatypes

import Lang.Datatypes
import Lang.Types
import Lang.Linux
import Lang.Debug
import Lang.BasicFunctions

test1_identifier = do
    doc "input: single letter 'n'"
    x86 $ callLabel "read_head_w8"
    assertPtop 1 "read_head_w8 failed\n"
    pdrop 1
    x86 $ callLabel "parse_identifier"

    assertPtop 2 "Top should be 2 (parse_successful).\n"
    pdrop 1

    assertPtop 1 "The identifier length should be 1.\n"
    pdrop 1

    assertPtopW8 0x6E "The 1 char should be n\n"
    pdropW8 1   

test2_identifier = do
    doc "input: single letter '0'"
    x86 $ callLabel "read_head_w8"
    assertPtop 1 "read_head_w8 failed\n"
    pdrop 1
    x86 $ callLabel "parse_identifier"

    assertPtop 1 "Top should be 1 (parse failure, input not matched.).\n"
    pdrop 1

    assertPtopW8 0x30 "The continuation char should be 0x30.\n"
    pdropW8 1

test3_identifier = do
    doc "input: string 'test'"
    callBody "read_head_w8"
    assertPtop 1 "read_head_w8 failed\n"
    pdrop 1
    callBody "parse_identifier"

    assertPtop 2 "Top should be 2 (parse success, continue parsing.).\n"
    pdrop 1

    assertPtop 4 "The identifier length should be 4.\n"
    pdrop 1

    assertPtopW8 0x74 "t"
    pdropW8 1
    assertPtopW8 0x73 "s"
    pdropW8 1
    assertPtopW8 0x65 "e"
    pdropW8 1
    assertPtopW8 0x74 "t"
    pdropW8 1

test4_stack_param = do
    doc "input: string ':w64 abc'"
    callBody "read_head_w8"
    assertPtop 1 "read_head_w8 failed\n"
    pdrop 1
    callBody "PARSE_STACK_PARAM"

    assertPtop 2 "Top should be 2 (parse success, continue parsing.).\n"
    pdrop 1

    callBody "dbg_dump_ptop_64"
    assertPtop (fnv1Integral "abc") "abc\n"
    pdrop 1

    callBody "dbg_dump_ptop_64"
    assertPtop (fnv1Integral "w64") "w64\n"
    pdrop 1

defineDbgParseTestSuite :: Lang ()
defineDbgParseTestSuite = defFunBasic "dbg_parse_test_suite" body
  where
    body = do
        doc "Test suite for parsers."
        test1_identifier
        test2_identifier
        test3_identifier

        test4_stack_param
        -- test5_stack_param

        x86 $ ret
