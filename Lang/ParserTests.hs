module Lang.ParserTests where

import Lang.Parsing 

import X86.Datatypes
import X86.X86
import ASM.Datatypes
import ASM.ASM

import Lang.BasicFunctions
import Lang.Debug
import Lang.Linux

import Data.Word

data ParseTest = ParseTest
    String  -- Parser label (function name)
    String  -- Parser stdin input that is passed to the parser
    Word32  -- Expected parser result, a w64 on the stack.
    Char    -- If the parser doesn't fail, expected continuation w8.
    [Either Word64 Word8] -- Literals that must be present on the stack 
                          -- on success: the data that the parser leaves
                          -- behind.
        
resultAscii = Right . ascii
resultW8    = Right 

getInput (ParseTest _ s _ _ _) = s

parserTestSuiteStdin :: [String]
parserTestSuiteStdin = map getInput parserTests

testStack :: [Either Word64 Word8] -> X86_64 ()
testStack [] = return ()
testStack ((Right w8):xs) = do
    assertPtopW8 (fromIntegral w8) "Unexpected stack data w8"
    pdropW8 1
    testStack xs
testStack ((Left w64):xs) = do
    assertPtop (fromIntegral w64) "Unexpected stack data w64"
    pdrop 1
    testStack xs

parseTestSuiteGen [] = return ()
parseTestSuiteGen (
    (ParseTest lbl inp expected_result expected_cont_chr xs):rest) = do
    doc $ "Testing parser " ++ lbl ++ " with input " ++ inp
    writeMsgHelper $ "Testing parser " ++ lbl ++ " with input " ++ inp ++ "\n"

    doc "Add a magical value on the stack to make sure that we find it"
    doc "there after the test has completed. The test should not leave"
    doc "things on the stack."
    ppush $ I32 0xCAFE

    callLabel "read_head_w8"
    assertPtop 1 "read_head_w8 failed!"
    pdrop 1

    doc $ "Call parser " ++ lbl
    callLabel lbl

    assertPtop (fromIntegral expected_result) $ "Unexpected parser result" 
    ppop rax
 
    cmp rax $ I32 parse_success
    jeNear succLabel
    cmp rax $ I32 parse_reject
    jeNear rejeLabel
    cmp rax $ I32 parse_fail
    jeNear failLabel

    doc "In case of success or rejection, test the continuation char:"
    setLabel succLabel
    setLabel rejeLabel

    cmp contCharR8 (asciiI32 expected_cont_chr)
    jneNear cchrLabel
    {-
    assertPtopW8 
        (fromIntegral (ascii expected_cont_chr))
        "Unexpected continuation char (w8)" 
    pdropW8 1
    -}

    testStack xs
    
    doc "Test that our magic constant is present on the stack, i.e. that"
    doc "no stack polution occurred after the parser has completed its run."
    
    assertPtop 0xCAFE "Stack polution detected"
    pdrop 1

    jmpLabel passLabel

    setLabel cchrLabel
    writeMsgHelper "Continuation character failure! (r8 register)\n"
    ret

    setLabel failLabel
    doc "In case of failure we don't need to test the continuation char."

    setLabel passLabel
    parseTestSuiteGen rest
  where
    failLabel = lbl ++ "_" ++ inp ++ "_fail"
    cchrLabel = lbl ++ "_" ++ inp ++ "_cchr"
    succLabel = lbl ++ "_" ++ inp ++ "_success"
    rejeLabel = lbl ++ "_" ++ inp ++ "_reject"
    passLabel = lbl ++ "_" ++ inp ++ "_pass"

testSuiteParsers = do
    doc "PARSERS TEST SUITE"
    parseTestSuiteGen parserTests

-- TODO: Do a check that the test coverage is 100%
-- TODO: Make the tests check the emitted code as well
parserTests = [
        ParseTest "parse_09" "?" parse_reject  '?'
            []
    ,   ParseTest "parse_09" "0?" parse_success '?'
            [resultAscii '0']
    ,   ParseTest "parse_09" "9?" parse_success '?'
            [resultAscii '9']
    ,   ParseTest "parse_09" "/" parse_reject '/'
            []
    ,   ParseTest "parse_09" ":" parse_reject ':'
            []
    ,   ParseTest "parse_ws" "?" parse_reject  '?'
            []
    ,   ParseTest "parse_ws" " ?" parse_success '?'
            []
    ,   ParseTest "parse_wss" "?" parse_success '?'
            []
    ,   ParseTest "parse_wss" "   ?" parse_success '?'
            []
    ,   ParseTest "parse_az09_"      "a?"    parse_success '?'
            [resultAscii 'a']
    ,   ParseTest "parse_az09_"      "z?"    parse_success '?'
            [resultAscii 'z']
    ,   ParseTest "parse_az09_"      "`"     parse_reject  '`'
            []
    ,   ParseTest "parse_az09_"      "{"     parse_reject  '{'
            []
    ,   ParseTest "parse_identifier" "abc?"  parse_success '?'
            [Left 3, Right (ascii 'c'), Right (ascii 'b'), Right (ascii 'a')]
    ,   ParseTest "parse_identifier" "a?"    parse_success '?'
            [Left 1, Right (ascii 'a')]
    ,   ParseTest "parse_identifier" "a_?"   parse_success '?'
            [Left 2, Right (ascii '_'), Right (ascii 'a')]
    ,   ParseTest "parse_identifier" "a0?"   parse_success '?'
            [Left 2, Right (ascii '0'), Right (ascii 'a')]
    ,   ParseTest "parse_identifier" "a9?"   parse_success '?'
            [Left 2, Right (ascii '9'), Right (ascii 'a')]
    ,   ParseTest "parse_identifier" "a/"    parse_success '/'
            [Left 1, Right (ascii 'a')]
    ,   ParseTest "parse_identifier" "a:"    parse_success ':'
            [Left 1, Right (ascii 'a')]
    ,   ParseTest "parse_identifier" "`"     parse_reject  '`'
            []
    ,   ParseTest "parse_identifier" "{"     parse_reject  '{'
            []
    ,   ParseTest "parse_identifier" "0"     parse_reject  '0'
            []
    ,   ParseTest "parse_integer_w64"    "/"     parse_reject  '/'
            []
    ,   ParseTest "parse_integer_w64"    ":"     parse_reject  ':'
            []
    ,   ParseTest "parse_integer_w64"    "9?"    parse_success '?'
            [Left 9]
    ,   ParseTest "parse_integer_w64"    "0?"    parse_success '?'
            [Left 0]
    ,   ParseTest "parse_integer_w64"    "1234?" parse_success '?'
            [Left 1234]
    ,   ParseTest "parse_integer_w64"    "001234?" parse_success '?'
            [Left 1234]
    ,   ParseTest "parse_integer_w64"    "12a"   parse_success 'a'
            [Left 12]
    ,   ParseTest "parse_integer_w64" (show (2^32 - 1) ++ "?") parse_success '?'
            [Left $ fromIntegral $ 2^32 - 1] 
    ,   ParseTest "parse_integer_w64"    (show (2^32) ++ "?") parse_success '?'
            [Left $ fromIntegral $ 2^32]
    ,   ParseTest "parse_integer_w64"    (show (2^64 - 1) ++ "?") parse_success '?'
            [Left $ fromIntegral $ 2^64 - 1]
    ,   ParseTest "parse_integer_w64"    (show (2^64) ++ "?") parse_fail '?'
            []
    ,   ParseTest "parse_int_or_identifier" "push1 " parse_success ' '
            []
    ,   ParseTest "parse_int_or_identifier" "ident " parse_success ' '
            []
    ,   ParseTest "parse_int_or_identifier" "ret " parse_success ' '
            []
    -- ,   ParseTest "parse_int_or_identifier" "if 0 " parse_success ' '
    --        []
    ,   ParseTest "parse_int_or_identifier" "123 " parse_success ' '
            []
    ,   ParseTest "parse_rbrace" "}?" parse_success '?' 
            []
    ,   ParseTest "parse_rbrace" "?" parse_reject '?'
            []
    ,   ParseTest "parse_block" "?" parse_reject '?'
            []
    ,   ParseTest "parse_block" "{?" parse_fail '?'
            []
    ,   ParseTest "parse_block" "{}?" parse_success '?'
            []
    ,   ParseTest "parse_block" "{ d1 123 d2 d3 }?" parse_success '?'
            []
    ,   ParseTest "parse_block" "{d1 123 d2 d3 }?" parse_success '?'
            []
    ,   ParseTest "parse_block" "{d1 123 d2 d3}?" parse_success '?'
            []
    ,   ParseTest "parse_block" "{d1 123 d2 d3} " parse_success ' '
            []
    ,   ParseTest "parse_if_term" "?" parse_reject '?'
            []
    ,   ParseTest "parse_if_term" "heyy?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "a?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "push1?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "0?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "1337?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "{push1}?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "{0 1 2}?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "#A1?" parse_success '?'
            []
    ,   ParseTest "parse_if_term_wss" "1337  ?" parse_success '?'
            []
    ,   ParseTest "parse_if_term_wss" "?" parse_reject '?'
            []
    ,   ParseTest "parse_if_term_wss" "test ?" parse_success '?'
            []
    ,   ParseTest "parse_if_term_wss" "test?" parse_success '?'
            []
    ,   ParseTest "parse_if_term_wsss" "?" parse_success '?'
            []
    ,   ParseTest "parse_if_term_wsss" "push1?" parse_success '?'
            []
    ,   ParseTest "parse_if_term_wsss" "push1 ?" parse_success '?'
            []
    ,   ParseTest "parse_if_term_wsss" "64?" parse_success '?'
            []
    ,   ParseTest "parse_if_term_wsss" "push1 64 push1?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "if_top_nz push1?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "if_top_nz if_top_nz push1?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "if_top_nz {d1 123 d2 d3}?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "if_top_nz{d1 123 d2 d3}?" parse_success '?'
            []
    ,   ParseTest "parse_if_term" "if_top_nz {?" parse_fail '?'
            []
    ,   ParseTest "parse_if_term" "if_top_nz ?" parse_fail '?'
            []
    ,   ParseTest "parse_if_term" "{push1 {push1}}?" 
            parse_success '?' []
    ,   ParseTest "parse_if_term" "if_top_nz {push1 if_top_nz {push1}}?" 
            parse_success '?' []
    ,   ParseTest "parse_def_body_end" "1.?" 
            parse_success '?' []
    ,   ParseTest "parse_def_body_end" "1 .?" 
            parse_success '?' []
    ,   ParseTest "parse_def_body_end" "1. " 
            parse_success ' ' [] -- The definition shouldn't consume whitespace
    ,   ParseTest "parse_def_body" "=1.?" 
            parse_success '?' []
    ,   ParseTest "parse_def_body" "= 1.?" 
            parse_success '?' []
    ,   ParseTest "parse_def" "ab = 3.?" 
            parse_success '?' []
    ,   ParseTest "parse_def" "push2 = push1 push1 plus .?" 
            parse_success '?' []
    ,   ParseTest "parse_def" "push2 = push1 push1 plus . " 
            parse_success ' ' []
    ,   ParseTest "parse_norparen" "?)"
            parse_success ')' []
    ,   ParseTest "parse_norparen" ")"
            parse_reject ')' []
    ,   ParseTest "parse_comment" "()?"
            parse_success '?' []
    ,   ParseTest "parse_comment" "( For ye shall go out with joy... )?"
            parse_success '?' []
    ,   ParseTest "parse_af09_hex" "0?" 
            parse_success '?' [resultW8 0]
    ,   ParseTest "parse_af09_hex" "9?" 
            parse_success '?' [resultW8 9]
    ,   ParseTest "parse_af09_hex" "A?" 
            parse_success '?' [resultW8 10]
    ,   ParseTest "parse_af09_hex" "F?" 
            parse_success '?' [resultW8 15]
    ,   ParseTest "parse_integer_w8" "#A9?" 
            parse_success '?' [resultW8 0xA9]
    ,   ParseTest "parse_integer_w8" "#12?" 
            parse_success '?' [resultW8 0x12]
    ,   ParseTest "parse_integer_w8" "#00?" 
            parse_success '?' [resultW8 0x00]
    ,   ParseTest "parse_integer_w8" "#FF?" 
            parse_success '?' [resultW8 0xFF]
    ]

