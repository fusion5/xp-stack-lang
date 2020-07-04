module Lang.Parsing where

import X86.Datatypes
import X86.X86
import ASM.ASM
import ASM.Datatypes

import Data.Word

import Lang.BasicFunctions
import Lang.Debug
import Lang.Linux

parse_fail, parse_reject, parse_success :: Word32
parse_fail         = 0 -- Parser failed and parsing should be stopped.
parse_reject       = 1 -- Parser rejected the input. Use the specific character
                       -- that was rejected with the next parser.
parse_success      = 2 -- Parser succeeded and consumed all input. 
                       -- Further input should be requested.

-- Parses input. Skips whitespace, control characters, etc. 
-- Stops at the first non-such char and pushes it on the stack.
-- Type : -> :w8        :w64
-- Func : -> :read_char:success
{-
defineRdHeadW8 :: X86_64 ()
defineRdHeadW8 = defFunBasic "read_head_w8" body
  where
    body = do
        doc "Allocate 1 byte on the parameter-stack in which our"
        doc "first non-whitespace character is placed:"
        ppush $ I8 0x00

        asm $ setLabel "RPC_WHILE"
        doc "Please read"
        mov rax $ I64 $ fromIntegral linux_sys_read
        doc "1 char"
        mov rdx $ I64 0x01 
        doc "From stdin"
        xor rbx rbx
        doc "Into the pstack"
        mov rcx rsi
        int

        cmp rax $ I32 0x00
        jeNear "RPC_ERROR"

        xor rax rax
        ptopW8 al

        cmp rax $ I32 0x20 
        jleNear "RPC_WHILE" -- Space or Control character, skip
        cmp rax $ I32 0x7F
        jeNear  "RPC_WHILE" -- ESC, skip

        ppush $ I32 1 -- Success
        ret

        asm $ setLabel "RPC_ERROR"
        ppush $ I32 0 -- Fail
-}

-- Parses input. 
-- Type : -> :w8        :w64
-- Func : -> :read_char:success
defineRdHeadW8 :: X86_64 ()
defineRdHeadW8 = defFunBasic "read_head_w8" body
  where
    body = do
        doc "Allocate 1 byte on the parameter-stack in which our"
        doc "read character is placed:"
        ppush $ I8 0x00

        doc "Please read"
        mov rax $ I64 $ fromIntegral linux_sys_read
        doc "1 char"
        mov rdx $ I64 0x01 
        doc "From stdin"
        xor rbx rbx
        doc "Into the pstack"
        mov rcx rsi
        int

        cmp rax $ I32 0x00
        jeNear "RPC_ERROR"

        ppush $ I32 1 -- Success
        ret

        asm $ setLabel "RPC_ERROR"
        ppush $ I32 0 -- Fail

{-
defineRdTailW8 :: X86_64()
defineRdTailW8 = defFunBasic "read_tail_w8" body
  where
    body = do
        doc "READ_TAIL_W8 reads characters until a non-printable"
        doc "character is encountered."
        doc "It pushes on the stack an array of w8s then it pushes a"
        doc "w64 indicating how many chars have been read,"
        doc "followed by a w64 indicating success or failure."
        doc "r15 counts the chars that are successfully read."
        xor r15 r15
        asm $ setLabel "READ_TAIL_WHILE"

        doc "Allocate 1 byte on the stack to read into"
        ppush $ I8 0x00
        doc "Please read"
        mov rax $ I64 $ fromIntegral linux_sys_read
        doc "1 char"
        mov rdx $ I64 0x01 
        doc "From stdin"
        xor rbx rbx
        doc "Into the pstack"
        mov rcx rsi
        int

        cmp rax $ I32 0x00
        je "READ_TAIL_W8_ERROR"
       
        doc "Place the char we've just read into rax"
        xor rax rax
        ptopW8 al

        doc "Probe the char we just read if it's non-printable "
        doc "then break the loop:"
        cmp rax $ asciiI32 ' ' -- Space or less, skip
        jleNear "READ_TAIL_W8_BREAK" 
        cmp rax $ I32 0x7F -- ESC
        jeNear  "READ_TAIL_W8_BREAK"

        inc r15
        
        doc "Repeat"
        jmpLabel "READ_TAIL_WHILE"

        asm $ setLabel "READ_TAIL_W8_BREAK"
        doc "Free the stack from the last allocation:"
        ppopW8 al
        doc "Number of chars read:"
        ppush $ r15
        doc "Success:"
        ppush $ I32 1
        ret

        asm $ setLabel "READ_TAIL_W8_ERROR"
        doc "Free the stack from the last allocation:"
        ppopW8 al
        doc "Number of chars read:"
        ppush $ r15
        doc "Error:"
        ppush $ I32 0
        ret

defineRdTailAZW8 :: X86_64 ()
defineRdTailAZW8 = defFunBasic "read_tail_az_w8" body
  where
    body = do
        doc "READ_TAIL_AZ_W8 reads characters until a non[a-z,0-9,_]"
        doc "character is encountered."
        doc "It pushes on the stack an array of w8s then it pushes a"
        doc "w64 indicating how many chars have been read,"
        doc "followed by a w64 indicating success or failure."
        doc "r15 counts the chars that are successfully read."
        xor r15 r15
        asm $ setLabel "READ_TAIL_AZ_WHILE"

        doc "Allocate 1 byte on the stack to read into"
        ppush $ I8 0x00
        doc "Please read"
        mov rax $ I64 $ fromIntegral linux_sys_read
        doc "1 char"
        mov rdx $ I64 0x01 
        doc "From stdin"
        xor rbx rbx
        doc "Into the pstack"
        mov rcx rsi
        int

        cmp rax $ I32 0x00
        jeNear "READ_TAIL_AZ_W8_ERROR"
       
        doc "Place the char we've just read into rax"
        xor rax rax
        ptopW8 al

        doc "Probe the char we just read if it's non-a-z "
        doc "then break the loop:"
        cmp rax $ asciiI32 'a'
        jlNear "READ_TAIL_AZ_W8_NON_AZ" 
        cmp rax $ asciiI32 'z'
        jgNear "READ_TAIL_AZ_W8_NON_AZ"

        doc "It's an a-z. Repeat"
        inc r15
        jmpLabel "READ_TAIL_AZ_WHILE"

        setLabel "READ_TAIL_AZ_W8_NON_AZ"
        cmp rax $ asciiI32 '0'
        jlNear "READ_TAIL_AZ_W8_NON_09" 
        cmp rax $ asciiI32 '9'
        jgNear "READ_TAIL_AZ_W8_NON_09"

        doc "It's a 0-9. Repeat"
        inc r15
        jmpLabel "READ_TAIL_AZ_WHILE"
       
        setLabel "READ_TAIL_AZ_W8_NON_09"
        cmp rax $ asciiI32 '_'
        jneNear "READ_TAIL_AZ_W8_NON__"

        doc "It's an underscore. Repeat"
        inc r15
        jmpLabel "READ_TAIL_AZ_WHILE"

        setLabel "READ_TAIL_AZ_W8_NON__"

        doc "Check whether it's a non-printable character."
        doc "If so then end with success."

        cmp rax $ asciiI32 ' ' -- Space or less, skip
        jleNear "READ_TAIL_W8_IDENTIFIER_SUCCESS" 
        cmp rax $ I32 0x7F -- ESC
        jeNear  "READ_TAIL_W8_IDENTIFIER_SUCCESS"

        ppopW8 al
        doc "Number of chars read:"
        ppush $ r15
        doc "Continuation character for the next parser:"
        ppush al
        ppush $ I32 parse_success
        ret

        setLabel "READ_TAIL_W8_IDENTIFIER_SUCCESS"
        doc "Drop the whitespace char and succeed."
        pdropW8 1

        doc "Number of chars read (part of the data):"
        ppush $ r15

        doc "Success:"
        ppush $ I32 parse_success
        ret

        setLabel "READ_TAIL_AZ_W8_ERROR"
        doc "Free the stack from the last allocation:"
        ppopW8 al
        doc "Error:"
        ppush $ I32 parse_fail
        ret
-}

callRequiredParser fromParser parserName = do
    callLabel parserName
    ppop rax
    cmp rax $ I32 parse_reject
    jeNear failLabel
    cmp rax $ I32 parse_fail
    jeNear failLabel
  where
    failLabel = fromParser ++ "_fail"

callOptionalParser fromParser parserName = do
    callLabel parserName
    ppop rax
    cmp rax $ I32 parse_fail
    jeNear failLabel
  where
    failLabel = fromParser ++ "_fail"
        
callStarParser fromParser subParserLabel = do
    setLabel loopLabel
    callLabel subParserLabel
    ppop rax

    doc "Has the parser succeeded? If so then again!"
    doc "A call to read_head_w8 isn't necessary because the"
    doc "sub-parser performs that."
    cmp rax $ I32 parse_success
    jeNear loopLabel

    cmp rax $ I32 parse_fail
    jeNear $ fromParser ++ "_fail"

    doc "Otherwise, "
    doc "the parser has rejected the input: it doesn't match."
    doc "But that doesn't mean that fromParser should fail, "
    doc "just do nothing, because that doesn't consitute a failure"
    doc "and we just need to call the next parser in the chain (if any)."
    doc "If there is no other parser in the chain, the control flow"
    doc "returns a success."

  where
    loopLabel = fromParser ++ "_loop_" ++ subParserLabel

-- Input  :w8 (a char taken from stdin)
-- Output :w64 parse_fail (on parse failure)
--     or :w8 continuation_char:w64 parse_reject (input rejection, no data!)
--     or :...data...:w8 continuation_char:w64 parse_success (on parse success)
-- Side effect, may consume input from stdin
genericParser :: String 
    -> (Val -> String -> X86_64 ())
    -> X86_64 () -> X86_64 () -> X86_64 ()
genericParser 
    parserName  -- The name of the parser (String)
    testInput   -- Test the continuation char against this parser
    subParsers  -- Optional or required parsers.
    dataProcess -- What to do once the input is fully accepted.
      = defFunBasic parserName body 
  where 
    body = do
        doc "Test whether this parser accepts the input."
        doc "If it doesn't, then jump to the _reject label."
        xor rax rax

        -- writeMsgHelper $ "Top char for parser " ++ parserName ++ "\n"
        -- callLabel "dbg_dump_ptop_w8"
        ppeerW8 0 al
        testInput rax parserName

        doc "The first char is acceptable: leave it on the stack,"
        doc "and read one more character to pass on to any sub-parsers:"

        callLabel "read_head_w8"
        -- assertPtop 1 "read_head_w8 failed\n"
        pdrop 1

        doc "Call the optional/required subparsers, if any."
        doc "They too are expected to leave a continuation character on the stack."
        doc "If any of the subparsers fails, they jump to the _fail point;"
        doc "thus, failure of a sub-parser is propagated to the parent parser."
        subParsers

        doc "No failure occured; process the input on the stack and succeed."
        doc "The processing code must leave the continuation character as the"
        doc "last character on the stack to observe the parser contract."
        doc "TODO: Think about whether it would be cleaner to keep the cont. char"
        doc "on the stack rather than to move it around on the stack..."
        doc "Of course, it is possible in general that the processing step "
        doc "might cause a failure and to jump to the _fail label. But it should "
        doc "never jump to _reject: only the testInput step should do that."
        dataProcess 

        doc "Successful processing: return success."
        ppush $ I32 parse_success
        ret

        setLabel $ parserName ++ "_reject"
        doc "Rejected input: this parser doesn't match the input."
        doc "The parser cannot even begin to process this input."
        ppush $ I32 parse_reject
        ret

        setLabel $ parserName ++ "_fail"
        doc "Malformed input: the parser has began to match the input,"
        doc "but it has rejected it resulting in a failure."
        ppush $ I32 parse_fail
        ret

decimalInputTest reg parserName = do
    doc "Is the char in the register in 0-9?"
    doc "If not, then reject"

    doc "Less than ASCII code 0?"
    cmp reg $ asciiI32 '0'
    jlNear  $ parserName ++ "_reject"

    doc "Is the char greater than ASCII code 9?"
    cmp reg $ asciiI32 '9'
    jgNear  $ parserName ++ "_reject"

azInputTest reg parserName = do
    doc "Is the char in the register in a-z?"
    doc "If not, then reject"

    doc "Less than ASCII code a?"
    cmp reg $ asciiI32 'a'
    jlNear  $ parserName ++ "_reject"

    doc "Is the char greater than ASCII code z?"
    cmp reg $ asciiI32 'z'
    jgNear  $ parserName ++ "_reject"

az09_InputTest reg parserName = do
    doc "Is the char in the register in {a..z, 0..9, _}?"
    doc "If not, then reject"
    cmp reg $ asciiI32 'a'
    jlNear  $ parserName ++ "_non_az"

    cmp reg $ asciiI32 'z'
    jgNear  $ parserName ++ "_non_az"

    doc "The input is within a-z, do not reject"
    jmpLabel $ parserName ++ "_ok"

    setLabel $ parserName ++ "_non_az"

    cmp reg $ asciiI32 '0'
    jlNear  $ parserName ++ "_non_09"

    cmp reg $ asciiI32 '9'
    jgNear  $ parserName ++ "_non_09"

    doc "The input is within 0-9, do not reject"
    jmpLabel $ parserName ++ "_ok"

    setLabel $ parserName ++ "_non_09"

    doc "Unless it's an underscore, reject the input."
    cmp reg $ asciiI32 '_'
    jneNear $ parserName ++ "_reject"

    setLabel $ parserName ++ "_ok"

wsInputTest reg parserName = do
    doc "Is the char ESC or greater? Then ok"
    cmp reg $ I32 0x7F --ESC
    jgNear $ parserName ++ "_ok"

    doc "Is the char space or less? Then ok"
    cmp reg $ asciiI32 ' '
    jleNear $ parserName ++ "_ok"    

    doc "It's not whitespace. Reject!"
    jmpLabel $ parserName ++ "_reject"

    setLabel $ parserName ++ "_ok"
    
defineParse09 :: X86_64 () 
defineParse09 = do
    doc "Parse a single char from {0-9} and inc. r15 (the char counter)"
    genericParser "parse_09" test subparsers process 
  where
    test reg parserName = do
        decimalInputTest reg parserName
        inc r15
    subparsers = return ()
    process    = return ()
    

defineParseAZ09_ :: X86_64 ()
defineParseAZ09_ = do
    doc "Parse a single char from {a-z, 0-9, _} anc inc. r15 (the char counter)"
    genericParser "parse_az09_" test subparsers process
  where
    test reg parserName = do
        az09_InputTest reg parserName
        inc r15
    subparsers = return ()
    process    = return ()

defineParseWhitespace :: X86_64 ()
defineParseWhitespace = do
    doc "Parse a single whitespace character without leaving it on the stack."
    genericParser "parse_ws" test subparsers process
  where
    test       = wsInputTest
    subparsers = return ()
    process = do
        doc "We do this to ignore the space."
        doc "Drop the space allowed by the input test" 
        ppopW8 al 
        doc "Drop the space allowed by the input test" 
        pdropW8 1 
        doc "Restore the continuation chr. on the stack"
        ppush al 

defineParseWhitespaceSeq :: X86_64 ()
defineParseWhitespaceSeq = do
    doc $ "Parse one or more whitespace characters without leaving them " ++
          "on the stack."
    genericParser "parse_wss" test subparsers process
  where
    test       = wsInputTest
    subparsers = callStarParser "parse_wss" "parse_ws"
    process = do
        doc "We do this to ignore the space."
        doc "Backup the continuation chr."
        ppopW8 al 
        doc "Drop the space allowed by the input test" 
        pdropW8 1
        doc "Restore the continuation chr. on the stack"
        ppush al 

defineParseIdentifier :: X86_64 ()
defineParseIdentifier = genericParser "parse_identifier" test subparsers process
  where
    test       = azInputTest 
    subparsers = do
        doc "Use r15 as a character counter"
        mov r15 $ I64 1
        callStarParser     "parse_identifier" "parse_az09_"
        callOptionalParser "parse_identifier" "parse_wss"
    process = do 
        doc "Increment the read_tail_az_w8 count to account for " 
        doc "the first character."
        doc "Backup the continuation chr."
        ppopW8 al 
        doc "Add the character count to the stack."
        ppush r15 
        doc "Restore the continuation chr. on the stack"
        ppush al  

defineParseInteger :: X86_64 ()
defineParseInteger = genericParser "parse_integer" test subparsers process where
    test       = decimalInputTest
    subparsers = do
        doc "Use r15 as a character counter, initialize it with 1."
        mov r15 $ I64 1 
        doc "parse_09 increments r15 on each successful parse."
        callStarParser     "parse_integer" "parse_09"
        callOptionalParser "parse_integer" "parse_wss"
    process = do
        doc "We have the 0-9 ascii chars on the stack and we wish to leave an"
        doc "integer there, of type w64. The parser might fail though, if the "
        doc "number is larger than a w64 can hold..."
        
        doc "Backup the continuation char into rbx"
        xor rbx rbx
        ppopW8 bl
        
        doc "rcx holds a power of 10 (multiplier). It starts from number 1."
        mov rcx $ I64 1

        doc "Store the parse result in r10 (which acts as an accumulator)"
        xor r10 r10

        setLabel "parse_integer_loop"
        do
            doc "If the end of the string is reached then end the loop"
            cmp r15 $ I32 0
            jeNear "parse_integer_loop_end"

            doc "Decrease the remaining chars counter"
            dec r15

            xor rax rax
            doc "Drop the 8-bit word from the pstack:"
            ppopW8 al

            doc "Convert from ascii to a base10 number by subtracting 0x30:"
            sub rax $ I32 0x30

            doc "Multiply rax by the current power of 10 stored in rcx."
            mul rcx
            joNear "parse_integer_fail"

            doc "Add rax to the r10 accumulator"
            doc "We use Jump Carry to detect overflow, since we assume"
            doc "that the integers that we are are adding are unsigned."
            add r10 rax
            jcNear "parse_integer_fail"

            doc "Multiply rcx by constant 10 (our base)"
            mov rdx $ I64 10
            mov rax rcx
            mul rdx
            -- joNear "parse_integer_fail"
            mov rcx rax
            
            jmpLabel "parse_integer_loop"
 
        setLabel "parse_integer_loop_end"
        
        doc "Push the multiplication result"
        ppush r10
        doc "Restore the continuation char"
        ppush bl


data ParseTest = ParseTest
    String  -- Parser label (function name)
    String  -- Parser stdin input that is passed to the parser
    Word32  -- Expected parser result, a w64 on the stack.
    Char    -- If the parser doesn't fail, expected continuation w8.
    [Either Word64 Word8] -- Literals that must be present on the stack 
                          -- on success: the data that the parser leaves
                          -- behind.
        
parserTests = [
        ParseTest "parse_ws"         "?"     parse_reject  '?'
            []
    ,   ParseTest "parse_ws"         " ?"    parse_success '?'
            []
    ,   ParseTest "parse_wss"         "?"    parse_reject  '?'
            []
    ,   ParseTest "parse_wss"         "   ?" parse_success '?'
            []
    ,   ParseTest "parse_az09_"      "a?"    parse_success '?'
            [Right (ascii 'a')]
    ,   ParseTest "parse_az09_"      "z?"    parse_success '?'
            [Right (ascii 'z')]
    ,   ParseTest "parse_az09_"      "`"     parse_reject  '`'
            []
    ,   ParseTest "parse_az09_"      "{"     parse_reject  '{'
            []
    ,   ParseTest "parse_identifier" "abc?"  parse_success '?'
            [Left 3, Right (ascii 'c'), Right (ascii 'b'), Right (ascii 'a')]
    ,   ParseTest "parse_identifier" "abc ?" parse_success '?'
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
    ,   ParseTest "parse_integer"    "/"     parse_reject  '/'
            []
    ,   ParseTest "parse_integer"    ":"     parse_reject  ':'
            []
    ,   ParseTest "parse_integer"    "9?"    parse_success '?'
            [Left 9]
    ,   ParseTest "parse_integer"    "0?"    parse_success '?'
            [Left 0]
    ,   ParseTest "parse_integer"    "0 ?"   parse_success '?'
            [Left 0]
    ,   ParseTest "parse_integer"    "9 ?"   parse_success '?'
            [Left 9]
    ,   ParseTest "parse_integer"    "1234?" parse_success '?'
            [Left 1234]
    ,   ParseTest "parse_integer"    "12a"   parse_success 'a'
            [Left 12]

    ,   ParseTest "parse_integer"    (show (2^32 - 1) ++ "?")   parse_success '?'
            [Left $ fromIntegral $ 2^32 - 1] 

    ,   ParseTest "parse_integer"    (show (2^32) ++ "?")       parse_success '?'
            [Left $ fromIntegral $ 2^32]

    ,   ParseTest "parse_integer"    (show (2^64 - 1) ++ "?")   parse_success '?'
            [Left $ fromIntegral $ 2^64 - 1]

    ,   ParseTest "parse_integer"    (show (2^64) ++ "?")       parse_fail '?'
            []
    ]

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
    assertPtopW8 
        (fromIntegral (ascii expected_cont_chr))
        "Unexpected continuation char (w8)" 
    pdropW8 1

    testStack xs
    
    doc "Test that our magic constant is present on the stack, i.e. that"
    doc "no stack polution occurred after the parser has completed its run."
    
    assertPtop 0xCAFE "Stack polution detected"
    pdrop 1

    setLabel failLabel
    doc "In case of failure we don't need to test the continuation char."

    parseTestSuiteGen rest
  where
    failLabel = lbl ++ "_" ++ inp ++ "_fail"
    succLabel = lbl ++ "_" ++ inp ++ "_success"
    rejeLabel = lbl ++ "_" ++ inp ++ "_reject"

testSuiteParsers = do
    doc "PARSERS TEST SUITE"
    parseTestSuiteGen parserTests
    

{-
defineParseIdentifier :: X86_64 ()
defineParseIdentifier = defFunBasic "parse_identifier" body
  where
    body = do
        doc "Inspect the first character from the pstack"
        xor rax rax
        ppeerW8 0 al

        -- callBody "dbg_dump_ptop_w8"

        doc "Is the char less than ascii code a?"
        cmp rax $ asciiI32 'a'
        jlNear "PARSE_IDENTIFIER_REJECT"

        doc "Is the char greater than ascii code z?"
        cmp rax $ asciiI32 'z'
        jgNear "PARSE_IDENTIFIER_REJECT"

        doc "Parsing a sequence of a-z,0-9 and _."
        doc "Place the rest of the input onto the stack..."
        callLabel "READ_TAIL_AZ_W8"
        ppop rax

        cmp rax $ I32 parse_success
        jeNear "PARSE_IDENTIFIER_SUCCESS"

        cmp rax $ I32 parse_reject
        jeNear "PARSE_IDENTIFIER_SUCCESS"

        doc "Failure.."
        ppush $ I32 parse_fail
        ret

        setLabel "PARSE_IDENTIFIER_SUCCESS"
        do 
            doc "Increment the count to account for the first character."
            ppopW8 al -- continuation character
            ppop rbx
            inc rbx
            ppush rbx
            ppush al

            ppush $ I32 parse_success
            ret

        setLabel "PARSE_IDENTIFIER_REJECT"
        ppush $ I32 parse_reject
        ret
-}
