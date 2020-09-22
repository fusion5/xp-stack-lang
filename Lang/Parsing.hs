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

noop = return ()

contCharR8 = r8

movContCharTo reg = do
    xor reg reg
    mov contCharR8 reg

ppopContChar = do
    ppop contCharR8

ppushContCharW8 = do
    cpush rax
    mov rax contCharR8
    ppush al
    cpop rax

defineParsers = do
    defineRdHeadW8
    defineParseIdentifier
    defineParseIdentifierWS
    defineParseAZ09_s
    defineParseAZ09_
    defineParseAZ_
    defineParseWhitespace
    defineParseWhitespaceSeq
    defineParse09
    defineParse09s
    defineParseInteger
    defineParseIfTerm
    defineParseIfTermWS
    defineParseIfTerms
    defineParseRBrace
    defineParseLBrace
    defineParseLBraceWS
    defineParseBlockEnd
    defineParseBlock
    defineParseIntOrIdentifier 
    defineParseEqual
    defineParseEqualWS
    defineParseDot
    defineParseDotWS
    defineParseDef
    defineParseDefBody
    defineParseDefBodyEnd

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

-- Reads 1 character of input into the r8 register. 
-- Type : -> :w64
-- Func : -> :success
defineRdHeadW8 :: X86_64 ()
defineRdHeadW8 = defFunBasic "read_head_w8" body
  where
    body = do
        doc "Allocate 8 bytes on the parameter-stack in which our"
        doc "read character is placed:"
        ppush $ I32 0x00

        doc "Please read"
        mov rax $ I64 $ fromIntegral linux_sys_read
        doc "1 char"
        mov rdx $ I64 0x01 
        doc "From stdin"
        xor rbx rbx
        doc "Into the pstack"
        mov rcx rsi
        int

        ppopContChar

        cmp rax $ I32 0x00
        jeNear "RPC_ERROR"

        ppush $ I32 1 -- Success
        ret

        asm $ setLabel "RPC_ERROR"
        ppush $ I32 0 -- Fail

callRequiredParser parserName failLabel = do
    callLabel parserName
    ppop rax
    cmp rax $ I32 parse_reject
    jeNear failLabel
    cmp rax $ I32 parse_fail
    jeNear failLabel

callOptionalParser parserName failLabel = do
    callLabel parserName
    ppop rax
    cmp rax $ I32 parse_fail
    jeNear failLabel
        
parseTerminal parserName testInput dataProcess = 
    defFunBasic parserName body
 where
    body = do
        doc "Parse a terminal that doesn't have any subparser."

        let rejectLabel = parserName ++ "_reject"
        let failLabel   = parserName ++ "_fail"

        testInput contCharR8 rejectLabel
        doc "The top char on the stack matches this parser:"

        dataProcess

        doc "Consume another char from the input, for the next parser."
        callLabel "read_head_w8"
        pdrop 1

        ppush $ I32 parse_success
        ret

        setLabel $ rejectLabel
        doc "Rejected input: this parser doesn't match the top"
        doc "character on the stack:"
        ppush $ I32 parse_reject

-- Parser "composer" A | B
parseAlt parserName parser1 process1 parser2 process2 =
    defFunBasic parserName body 
  where
    body = do
        doc $ "Try parser " ++ parser1 ++ " as part of a cascade."
        doc $ "If that rejects the input, then try parser " ++ parser2
        doc $ "If A and B are parsers then this does A | B."
        callBody parser1
        ppop rax

        doc "Has the first parser failed? Fail the sequence."
        failLabel <- asm $ freshLabelWithPrefix (
            "parser_seq_fail_" ++ parser1 ++ "_")
        cmp rax $ I32 parse_fail
        jeNear failLabel

        doc "Has the first parser rejected the input?"
        rejectLabel1 <- asm $ freshLabelWithPrefix (
            "parser_seq_reject_" ++ parser1 ++ "_")
        cmp rax $ I32 parse_reject
        jeNear rejectLabel1

        doc "The first parser accepted the input. Process it and return"
        doc "without calling the other parser."
        process1 failLabel
        ppush $ I32 parse_success
        ret

        setLabel rejectLabel1
        doc $ "Parser " ++ parser1 ++ " rejected its input--try " ++ parser2
        callBody parser2 
        doc $ "Evaluate the result from " ++ parser2    
        ppop rax

        rejectLabel2 <- asm $ freshLabelWithPrefix (
            "parser_seq_reject_" ++ parser2 ++ "_")
        doc $ "Has " ++ parser2 ++ " rejected its input or failed? Then also fail."
        cmp rax $ I32 parse_fail
        jeNear failLabel

        cmp rax $ I32 parse_reject
        jeNear rejectLabel2

        doc "The second parser accepted the input. Process it"
        process2 failLabel
        ppush $ I32 parse_success
        ret

        setLabel failLabel
        doc $ "Parser " ++ parser1 ++ " | " ++ parser2 ++ " has failed!"
        ppush $ I32 parse_fail
        ret

        setLabel rejectLabel2
        ppush $ I32 parse_reject
        ret

-- Parser "composer" A*
parseStar parserName subParserName init dataProcess =
    defFunBasic parserName body
  where
    body = do
        doc $ "Parse a 0..n sequence of parser " ++ subParserName

        doc "Initialise the state"
        init
        doc "Begin the loop"
        setLabel loopLabel
        do
            doc "Call the subparser"
            callLabel subParserName

            doc "Retrieve the last parse result from the stack"
            ppop rax

            doc "Has the subparser succeeded? If so then try again!"
            doc "(A call to read_head_w8 isn't necessary because the"
            doc "sub-parser performs that.)"
            cmp rax $ I32 parse_success
            jeNear loopLabel
        doc "End loop"

        doc "The subparser has failed. Fail the star parser immediately."
        cmp rax $ I32 parse_fail
        jeNear failLabel

        doc "Otherwise the last parser has rejected the input."
        doc "Before returning, call dataProcess."
        dataProcess

        doc "The star parser is successful, even though the last parser has"
        doc "rejected the input."
        ppush $ I32 parse_success
        ret

        setLabel failLabel
        ppush $ I32 parse_fail

    loopLabel  = parserName ++ "_loop"
    failLabel  = parserName ++ "_fail"

-- Parser "composer" A B
parseSequence parserName subParser1 subParser2 init process = 
    defFunBasic parserName body
  where
    failLabel   = parserName ++ "_fail"
    rejectLabel = parserName ++ "_reject"
    body = do
        doc $ "First call " ++ subParser1 ++ " and then call " ++ 
            subParser2 ++ ". Both are expected to succeed. "

        doc "Initialisation before parsing starts"
        init 

        callLabel subParser1
        doc $ "Retrieve the result of "++subParser1++" from the stack"
        ppop rax

        cmp rax $ I32 parse_fail
        jeNear failLabel

        cmp rax $ I32 parse_reject
        jeNear rejectLabel

        doc "Try the next parser, which must succeed as well."
        callLabel subParser2
        doc $ "Retrieve the result of "++subParser2++" from the stack"
        ppop rax

        cmp rax $ I32 parse_fail
        jeNear failLabel

        doc "If the second parser rejects, then fail because"
        doc "the first parser matching imples the second one too."
        cmp rax $ I32 parse_reject
        jeNear failLabel

        doc "Both parsers have succeeded."
        process 

        ppush $ I32 parse_success
        ret

        setLabel failLabel
        ppush $ I32 parse_fail
        ret

        setLabel rejectLabel
        ppush $ I32 parse_reject
        ret 
    
{- acceptAll reg parserName = do
    doc "Accept all input (do not test)" -}

inputTest_decimal reg rejectLabel = do
    doc "Is the char in the register in 0-9?"
    doc "If not, then reject"

    doc "Less than ASCII code 0?"
    cmp reg $ asciiI32 '0'
    jlNear  $ rejectLabel

    doc "Is the char greater than ASCII code 9?"
    cmp reg $ asciiI32 '9'
    jgNear  $ rejectLabel

inputTest_az reg rejectLabel = do
    doc "Is the char in the register in a-z?"
    doc "If not, then reject"

    doc "Less than ASCII code a?"
    cmp reg $ asciiI32 'a'
    jlNear  $ rejectLabel

    doc "Is the char greater than ASCII code z?"
    cmp reg $ asciiI32 'z'
    jgNear  $ rejectLabel

inputTest_az09 reg parserName = do
    doc "Is the char from the register in {a..z, 0..9}?"
    doc "If not, then reject"
    cmp reg $ asciiI32 'a'
    jlNear  $ parserName ++ "_non_az"

    cmp reg $ asciiI32 'z'
    jgNear  $ parserName ++ "_non_az"

    doc "The input is within a-z, do not reject"
    jmpLabel $ parserName ++ "_ok"

    setLabel $ parserName ++ "_non_az"

    cmp reg $ asciiI32 '0'
    jlNear  $ parserName ++ "_reject"

    cmp reg $ asciiI32 '9'
    jgNear  $ parserName ++ "_reject"

    doc "The input is within 0-9, do not reject"
    setLabel $ parserName ++ "_ok"

inputTest_az_ reg rejectLabel = do
    doc "Is the char in the register in {a..z, _}?"
    doc "If not, then reject"
    nonAZLabel <- asm $ freshLabelWithPrefix "inputTest_az__non_az_"
    okLabel    <- asm $ freshLabelWithPrefix "inputTest_az__ok_"
    cmp reg $ asciiI32 'a'
    jlNear nonAZLabel

    cmp reg $ asciiI32 'z'
    jgNear nonAZLabel

    doc "The input is within a-z, do not reject"
    jmpLabel okLabel

    setLabel nonAZLabel
    doc "Unless it's an underscore, reject the input."
    cmp reg $ asciiI32 '_'
    jneNear rejectLabel

    setLabel okLabel

inputTest_az09_ reg rejectLabel = do
    doc "Is the char in the register in {a..z, 0..9, _}?"
    doc "If not, then reject"
    nonAZLabel <- asm $ freshLabelWithPrefix "inputTest_az09__non_az_"
    non09Label <- asm $ freshLabelWithPrefix "inputTest_az09__non_09_"
    okLabel    <- asm $ freshLabelWithPrefix "inputTest_az09__ok_"
    cmp reg $ asciiI32 'a'
    jlNear nonAZLabel

    cmp reg $ asciiI32 'z'
    jgNear nonAZLabel

    doc "The input is within a-z, do not reject"
    jmpLabel okLabel

    setLabel nonAZLabel

    cmp reg $ asciiI32 '0'
    jlNear non09Label

    cmp reg $ asciiI32 '9'
    jgNear non09Label

    doc "The input is within 0-9, do not reject"
    jmpLabel okLabel

    setLabel non09Label

    doc "Unless it's an underscore, reject the input."
    cmp reg $ asciiI32 '_'
    jneNear rejectLabel

    setLabel okLabel

inputTestWS reg rejectLabel = do
    doc "Is the char ESC or greater? Then ok"
    cmp reg $ I32 0x7F --ESC
    
    okLabel <- asm $ freshLabelWithPrefix "wsInputTest_ok_"
    jgNear okLabel

    doc "Is the char space or less? Then ok"
    cmp reg $ asciiI32 ' '
    jleNear okLabel

    doc "It's not whitespace. Reject!"
    jmpLabel rejectLabel

    setLabel okLabel

inputTestRBrace reg rejectLabel = do
    doc "Is the char }?"
    cmp reg $ asciiI32 '}'
    jneNear rejectLabel

inputTestLBrace reg rejectLabel = do
    doc "Is the char {?"
    cmp reg $ asciiI32 '{'
    jneNear rejectLabel

inputTestEqual reg rejectLabel = do
    doc "Is the char =?"
    cmp reg $ asciiI32 '='
    jneNear rejectLabel

inputTestDot reg rejeLabel = do
    cmp reg $ asciiI32 '.'
    jneNear rejeLabel

defineParseDot = do
    parseTerminal "parse_dot" inputTestDot noop

defineParseDotWS = do
    parseSequence "parse_dot_wss"
        "parse_dot" "parse_wss" noop noop

defineParseEqual = do
    parseTerminal "parse_equal" inputTestEqual noop

defineParseEqualWS = do
    parseSequence "parse_equal_wss"
        "parse_equal" "parse_wss" noop noop

defineParseRBrace = do
    parseTerminal "parse_rbrace" inputTestRBrace noop

defineParseLBrace = do
    parseTerminal "parse_lbrace" inputTestLBrace noop

defineParseLBraceWS = do
    parseSequence "parse_lbrace_ws"
        "parse_lbrace" "parse_wss" noop noop

defineParseBlockEnd = do
    parseSequence "parse_block_end" 
        "parse_emit_if_terms" "parse_rbrace" noop noop

defineParseBlock = do
    parseSequence "parse_block"
        "parse_lbrace_ws" "parse_block_end" noop noop
    
defineParse09 :: X86_64 () 
defineParse09 = do
    doc "Parse a single char from {0-9} and inc. r15 (the char counter)"
    parseTerminal "parse_09" test process 
  where
    test = inputTest_decimal 
    process = do
        inc r15
        doc "Place the continuation char to the stack as a w8"
        ppushContCharW8

defineParse09s :: X86_64 () 
defineParse09s = parseStar "parse_09s" "parse_09" noop noop 

defineParseAZ09_ :: X86_64 ()
defineParseAZ09_ = do
    doc "Parse a single char from {a-z, 0-9, _} anc inc. r15 (the char counter)"
    parseTerminal "parse_az09_" test process
  where
    test = inputTest_az09_
    process = do
        inc r15
        doc "Place the continuation char to the stack as a w8"
        ppushContCharW8

defineParseAZ_ :: X86_64 ()
defineParseAZ_ = do
    doc "Parse a single char from {a-z, _} anc inc. r15 (the char counter)"
    parseTerminal "parse_az_" test process
  where
    test = inputTest_az_
    process = do
        inc r15
        doc "Place the continuation char to the stack as a w8"
        ppushContCharW8

defineParseWhitespace :: X86_64 ()
defineParseWhitespace = do
    doc "Parse a single whitespace character without leaving it on the stack."
    parseTerminal "parse_ws" inputTestWS noop 

defineParseWhitespaceSeq = 
    parseStar "parse_wss" "parse_ws" noop noop 

defineParseAZ09_s :: X86_64 ()
defineParseAZ09_s = parseStar "parse_az09_s" "parse_az09_" noop noop 

defineParseIdentifier = do
    doc "identifier = [a-z_][a-z0-9_]*"
    parseSequence "parse_identifier" "parse_az_" "parse_az09_s" init process
  where
    init = do
        doc "Use r15 as a character counter. Initialize to 0."
        doc "parse_az_ and parse_az09_ increment r15 each time they get called."
        mov r15 $ I64 0
    process = do
        ppush r15

defineParseIdentifierWS = 
    parseSequence "parse_identifier_wss" "parse_identifier" "parse_wss" noop noop
   
defineParseInteger = do
    doc "[0-9][0-9]*"
    parseSequence "parse_integer" "parse_09" "parse_09s" init process
  where
    init = do
        doc "Use r15 as a character counter, initialize it with 0."
        mov r15 $ I64 0
        doc "parse_09 increments r15 on each successful parse."
    process = do
        doc "We have the 0-9 ascii chars on the stack and we wish to leave an"
        doc "integer there, of type w64. The parser might fail though, if the "
        doc "number is larger than a w64 can hold..."
        
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
            -- TODO: Prove that this cannot overflow if we check 
            -- the add overflow below.
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
            -- TODO: Check how mul reports overflow!
            -- joNear "parse_integer_fail"
            mov rcx rax
            
            jmpLabel "parse_integer_loop"
 
        setLabel "parse_integer_loop_end"
        
        doc "Push the multiplication result"
        ppush r10
   
defineParseIfTermWS = 
    parseSequence "parse_emit_if_term_wss" 
        "parse_emit_if_term" "parse_wss" noop noop

defineParseIfTerms = 
    parseStar "parse_emit_if_terms" 
        "parse_emit_if_term_wss" noop noop

defineParseIfTerm = 
    parseAlt "parse_emit_if_term"
        "parse_block" processBlock
        "parse_emit_int_or_identifier" processIntOrIdentifier
  where
    processBlock _           = noop
    processIntOrIdentifier _ = noop
        
defineParseIntOrIdentifier = 
    parseAlt "parse_emit_int_or_identifier" 
        "parse_integer"    processInteger
        "parse_identifier" processIdentifier
  where
    processInteger _ = do
        callLabel "emit_lit_64"
    processIdentifier failLabel = do
        doc "parse_identifier has succeeded."
        doc "Check the kind of identifier that we have..."
        -- writeMsgHelper "Identifier case\n"

        callLabel "term_hash"
        doc "Place the hash in rax"
        ppop rax 

        doc "Now test our identifier against various options:"
        doc "Does it equal the hash of 'if'?"
        mov rbx (I64 $ fnv1s "if_top_nz")
        cmp rax rbx
        jneNear "parse_if_term_not_if"

        -- writeMsgHelper "If term\n"
        doc "Parsing an if term!"

        do 
            doc "Consume any whitespace after the if-term"
            callOptionalParser "parse_wss" failLabel

            callBody "emit_if_start"

            doc "Parse the if body, which could be anything from a simple"
            doc "term to a block term to another if term:"

            callRequiredParser "parse_emit_if_term" failLabel

            doc "(Note: it is critical that the program returns here"
            doc "to the same stack point as at the emit_if_start call "
            doc "because emit_if_end uses the value placed on the stack "
            doc "by emit_if_start)!"

            callBody "emit_if_end"
            jmpLabel "parse_if_term_done"

        setLabel "parse_if_term_not_if"

        doc "Try out whether it is a return statement? (ret)"
        mov rbx (I64 $ fnv1s "ret")
        cmp rax rbx
        jneNear "parse_if_term_not_ret"

        doc "Parsing a 'ret' keyword!"
        do
            -- doc "Consume any whitespace after the ret-term"
            -- callOptionalParser "parse_wss" failLabel
            callBody "emit_ret"
            jmpLabel "parse_if_term_done"

        setLabel "parse_if_term_not_ret"

        doc "Is it an identifier found in the "
        doc "environment?"
        do
            -- doc "Consume any whitespace after the term"
            -- callOptionalParser "parse_wss" failLabel

            ppush rax
            callLabel "term_look"
            ppop rax
            cmp rax (I32 0)
            jeNear "parse_if_term_undefined"

            doc "Found the term in the dictionary! Emit a call to the address."
            callLabel "emit_call"

            jmpLabel "parse_if_term_done"

        setLabel "parse_if_term_undefined"
        do
            writeMsgHelper "Undefined term skipped! Parse failure!\n"
            doc "Note we could also have the parser ignore the failure..."
            jeNear failLabel


        setLabel "parse_if_term_done"


defineParseDefBodyEnd = parseSequence "parse_def_body_end"
    "parse_emit_if_terms" "parse_dot" noop noop

defineParseDefBody = parseSequence "parse_def_body"
    "parse_equal_wss" "parse_def_body_end" noop noop

-- This is like a parseSequence but some action is performed after
-- the first parser succeeds.
defineParseDef = defFunBasic parserName body
  where
    parserName  = "parse_def"
    failLabel   = parserName ++ "_fail"
    failLabel2  = parserName ++ "_fail_dealloc"
    rejectLabel = parserName ++ "_reject"
    body = do
        doc $ "Parse a definition; parse the term"

        callLabel "parse_identifier_wss"
        ppop rax
        cmp rax $ I32 parse_reject
        jeNear rejectLabel
        cmp rax $ I32 parse_fail
        jeNear failLabel

        doc "Build a new [ prev ][ name hash ][ addr ] record on the stack"
        doc "for the new dictionary definition."

        doc "At this point the stack contains the term parsed by "
        doc "parse_identifier. Hash it and save the hash in rax."

        callBody "term_hash"
        ppop rax

        doc "Backup the current r11 in rbx"
        mov rbx r11
        
        doc "Advance r11 to make space for the new record"
        add r11 $ I32 24 
        doc "save [prev] (the previous dictionary entry) from the backup"
        mov (derefOffset r11 0)  rbx 
        doc "save [name hash]"
        mov (derefOffset r11 8)  rax 
        doc "save [addr] the address of the definition body (the current r9)"
        mov (derefOffset r11 16) r9  
    
        callLabel "parse_def_body" 
        ppop rax
        cmp rax $ I32 parse_fail
        jeNear failLabel2
        cmp rax $ I32 parse_reject
        jeNear failLabel2

        doc "Add a return statement at the end of the definition body"
        doc "to guarantee reentry after body execution..."
        callLabel "emit_ret"
        ppush $ I32 parse_success
        ret

        setLabel rejectLabel
        ppush $ I32 parse_reject
        ret

        setLabel failLabel
        ppush $ I32 parse_fail
        ret

        setLabel failLabel2
        doc "Restore the dictionary state from before the failed def attempt:"
        sub r11 $ I32 24 -- Dealocate the failed record
        ppush $ I32 parse_fail
        ret
        

{-
defineParseDef = parseSequence "parse_def" 
    "parse_identifier_wss" "parse_def_body" init process
  where
    init    = do

    process = do
        doc "Build a new [ prev ][ name hash ][ addr ] record on the stack"
        doc "for the new dictionary definition."

        doc "At this point the stack contains the term parsed by "
        doc "parse_identifier."
        doc "Hash it and save the hash in rax."

        callBody "term_hash"
        ppop rax

        doc "Backup the current r11 in rbx"
        mov rbx r11
        
        doc "Advance r11 to make space for the new record"
        add r11 $ I32 24 
        doc "save [prev] (the previous dictionary entry) from the backup"
        mov (derefOffset r11 0)  rbx 
        doc "save [name hash]"
        mov (derefOffset r11 8)  rax 
        doc "save [addr] the address of the definition body (the current r9)"
        mov (derefOffset r11 16) r9  
-}
        
data ParseTest = ParseTest
    String  -- Parser label (function name)
    String  -- Parser stdin input that is passed to the parser
    Word32  -- Expected parser result, a w64 on the stack.
    Char    -- If the parser doesn't fail, expected continuation w8.
    [Either Word64 Word8] -- Literals that must be present on the stack 
                          -- on success: the data that the parser leaves
                          -- behind.
        
resultAscii = Right . ascii

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
            [Right (ascii 'a')]
    ,   ParseTest "parse_az09_"      "z?"    parse_success '?'
            [Right (ascii 'z')]
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
    ,   ParseTest "parse_integer"    "/"     parse_reject  '/'
            []
    ,   ParseTest "parse_integer"    ":"     parse_reject  ':'
            []
    ,   ParseTest "parse_integer"    "9?"    parse_success '?'
            [Left 9]
    ,   ParseTest "parse_integer"    "0?"    parse_success '?'
            [Left 0]
    ,   ParseTest "parse_integer"    "1234?" parse_success '?'
            [Left 1234]
    ,   ParseTest "parse_integer"    "001234?" parse_success '?'
            [Left 1234]
    ,   ParseTest "parse_integer"    "12a"   parse_success 'a'
            [Left 12]
    ,   ParseTest "parse_integer" (show (2^32 - 1) ++ "?") parse_success '?'
            [Left $ fromIntegral $ 2^32 - 1] 
    ,   ParseTest "parse_integer"    (show (2^32) ++ "?") parse_success '?'
            [Left $ fromIntegral $ 2^32]
    ,   ParseTest "parse_integer"    (show (2^64 - 1) ++ "?") parse_success '?'
            [Left $ fromIntegral $ 2^64 - 1]
    ,   ParseTest "parse_integer"    (show (2^64) ++ "?") parse_fail '?'
            []
    ,   ParseTest "parse_emit_int_or_identifier" "push1 " parse_success ' '
            []
    ,   ParseTest "parse_emit_int_or_identifier" "ident " parse_success ' '
            []
    ,   ParseTest "parse_emit_int_or_identifier" "ret " parse_success ' '
            []
    -- ,   ParseTest "parse_emit_int_or_identifier" "if 0 " parse_success ' '
    --        []
    ,   ParseTest "parse_emit_int_or_identifier" "123 " parse_success ' '
            []
    ,   ParseTest "parse_rbrace" "}?" parse_success '?' 
            []
    ,   ParseTest "parse_rbrace" "?" parse_reject '?'
            []
    ,   ParseTest "parse_block" "?" parse_reject '?'
            []
    ,   ParseTest "parse_block" "{?" parse_fail '?'
            []
    ,   ParseTest "parse_block" "{ d1 123 d2 d3 }?" parse_success '?'
            []
    ,   ParseTest "parse_block" "{d1 123 d2 d3 }?" parse_success '?'
            []
    ,   ParseTest "parse_block" "{d1 123 d2 d3}?" parse_success '?'
            []
    ,   ParseTest "parse_block" "{d1 123 d2 d3} " parse_success ' '
            []
    ,   ParseTest "parse_emit_if_term" "?" parse_reject '?'
            []
    ,   ParseTest "parse_emit_if_term" "heyy?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "a?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "push1?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "0?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "1337?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "{push1}?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "{0 1 2}?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term_wss" "1337  ?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term_wss" "?" parse_reject '?'
            []
    ,   ParseTest "parse_emit_if_term_wss" "test ?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term_wss" "test?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_terms" "?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_terms" "push1?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_terms" "push1 ?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_terms" "64?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_terms" "push1 64 push1?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "if_top_nz push1?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "if_top_nz if_top_nz push1?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "if_top_nz {d1 123 d2 d3}?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "if_top_nz{d1 123 d2 d3}?" parse_success '?'
            []
    ,   ParseTest "parse_emit_if_term" "if_top_nz {?" parse_fail '?'
            []
    ,   ParseTest "parse_emit_if_term" "if_top_nz ?" parse_fail '?'
            []
    ,   ParseTest "parse_emit_if_term" "{push1 {push1}}?" 
            parse_success '?' []
    ,   ParseTest "parse_emit_if_term" "if_top_nz {push1 if_top_nz {push1}}?" 
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
--            [Left 2, Right (ascii 'b'), Right (ascii 'a')]
    ,   ParseTest "parse_def" "push2 = push1 push1 plus .?" 
            parse_success '?' []
    ,   ParseTest "parse_def" "push2 = push1 push1 plus . " 
            parse_success ' ' []
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
