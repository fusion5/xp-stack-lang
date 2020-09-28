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
    defineParseAZ09_s
    defineParseAZ09_
    defineParseAZ_
    defineParseWhitespace
    defineParseWhitespaceSeq
    -- defineParseZero
    defineParseHash
    defineParse09
    defineParse09s
    defineParseIntegerW64
    defineParseInteger
    defineParseIfTerm
    defineParseIfTermWS
    defineParseIfTerms
    defineParseRBrace
    defineParseLBrace
    defineParseRParen
    defineParseLParen
    defineParseBlockEnd
    defineParseBlock
    defineParseIntOrIdentifier 
    defineParseEqual
    defineParseDot
    defineParseDef
    defineParseDefBody
    defineParseDefBodyEnd
    defineParseNoRParen
    defineParseNoRParens
    defineParseComment
    defineParseCommentBody
    defineParseCommentOrBlock 
    defineParse09Hex
    defineParseAFHex
    defineParseAF09Hex 
    -- defineParseHexPrefix 
    defineParseHexBody 
    defineParseIntegerW8

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
        mov rbx $ I64 $ fromIntegral linux_stdin
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

-- Parser "composer" A B or A <wss> B
parseSequence withSpaceBetween parserName 
    init subParser1 midprocess subParser2 process = 
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

        if withSpaceBetween then
            callOptionalParser "parse_wss" failLabel
        else
            return ()
    
        midprocess

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

inputTest_AF reg rejectLabel = do
    doc "Is the char in the register in {A..F}"
    cmp reg $ asciiI32 'A'
    jlNear rejectLabel
    cmp reg $ asciiI32 'F'
    jgNear rejectLabel
    doc "The input is within A-F, do not reject"

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

inputTestNotChar chr reg rejectLabel = do
    doc $ "Is the char not " ++ show chr ++ "?"
    cmp reg $ asciiI32 chr
    jeNear rejectLabel

inputTestChar chr reg rejectLabel = do
    doc $ "Is the char " ++ show chr ++ "?"
    cmp reg $ asciiI32 chr
    jneNear rejectLabel

defineParseDot    = parseTerminal "parse_dot"    (inputTestChar '.') noop
defineParseEqual  = parseTerminal "parse_equal"  (inputTestChar '=') noop
defineParseLBrace = parseTerminal "parse_lbrace" (inputTestChar '{') noop
defineParseRBrace = parseTerminal "parse_rbrace" (inputTestChar '}') noop
defineParseLParen = parseTerminal "parse_lparen" (inputTestChar '(') noop
defineParseRParen = parseTerminal "parse_rparen" (inputTestChar ')') noop
defineParseNoRParen = parseTerminal "parse_norparen" (inputTestNotChar ')') noop
defineParseHash     = parseTerminal "parse_hash" (inputTestChar '#') noop
-- defineParseZero = parseTerminal "parse_zero" (inputTestChar '0') noop

defineParseBlockEnd = do
    parseSequence False "parse_block_end" 
        noop "parse_stmt_wsss" noop "parse_rbrace" noop

defineParseBlock = do
    parseSequence True "parse_block"
        noop "parse_lbrace" noop "parse_block_end" noop

defineParseCommentOrBlock =
    parseAlt "parse_comment_or_block"
        "parse_block"   processBlock
        "parse_comment" processComment
  where
    processBlock _ = noop
    processComment _ = noop
    
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
    doc "Parse a single char from {a-z, 0-9, _} and inc. r15 (the char counter)"
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
    parseSequence False "parse_identifier" 
        init "parse_az_" noop "parse_az09_s" process
  where
    init = do
        doc "Use r15 as a character counter. Initialize to 0."
        doc "parse_az_ and parse_az09_ increment r15 each time they get called."
        mov r15 $ I64 0
    process = do
        ppush r15

defineParseIntegerW64 = do
    doc "[0-9][0-9]*"
    parseSequence False "parse_integer_w64" 
        init "parse_09" noop "parse_09s" process
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
            joNear "parse_integer_w64_fail"

            doc "Add rax to the r10 accumulator"
            doc "We use Jump Carry to detect overflow, since we assume"
            doc "that the integers that we are are adding are unsigned."
            add r10 rax
            jcNear "parse_integer_w64_fail"

            doc "Multiply rcx by constant 10 (our base)"
            mov rdx $ I64 10
            mov rax rcx
            mul rdx
            -- TODO: Check how mul reports overflow!
            -- joNear "parse_integer_w64_fail"
            mov rcx rax
            
            jmpLabel "parse_integer_loop"
 
        setLabel "parse_integer_loop_end"
        
        doc "Push the multiplication result"
        ppush r10
   
defineParse09Hex :: X86_64 ()
defineParse09Hex = do
    parseTerminal "parse_09_hex" inputTest_decimal process
  where
    process = do
        doc "Place the continuation char to the stack as a w8"
        ppushContCharW8
        doc "Convert it to decimal 0-9"
        xor rax rax
        ppop al
        sub rax (I32 0x30)
        ppush al

defineParseAFHex :: X86_64 ()
defineParseAFHex = do
    parseTerminal "parse_af_hex" inputTest_AF process
  where
    process = do
        doc "Place the continuation char to the stack as a w8"
        ppushContCharW8
        doc "Convert it to decimal 10-15"
        xor rax rax
        ppop al
        sub rax (I32 55)
        ppush al

defineParseAF09Hex :: X86_64 ()
defineParseAF09Hex = do
    parseAlt "parse_af09_hex"
        "parse_09_hex" no_process
        "parse_af_hex" no_process
  where
    no_process _ = noop

defineParseHexBody :: X86_64 ()
defineParseHexBody = do
    parseSequence False "parse_hex_body" 
        init "parse_af09_hex" noop "parse_af09_hex" process
  where
    init    = noop
    process = do
        ppop bl
        ppop al
        doc "Shift 4 bits to the left"
        sal rax (I8 4)
        add rax rbx
        ppush al
        
{-
 - Removed because of ambiguity with integer 0
defineParseHexPrefix :: X86_64 ()
defineParseHexPrefix = do
    parseSequence False "parse_hex_prefix"
        noop "parse_zero" noop "parse_x" noop
-}

defineParseIntegerW8 :: X86_64 ()
defineParseIntegerW8 = do
    parseSequence False "parse_integer_w8"
        noop "parse_hash" noop "parse_hex_body" noop

defineParseIfTermWS = 
    parseSequence False "parse_stmt_wss" 
        noop "parse_stmt" noop "parse_wss" noop

defineParseIfTerms = 
    parseStar "parse_stmt_wsss" 
        "parse_stmt_wss" noop noop

defineParseIfTerm = 
    parseAlt "parse_stmt"
        "parse_comment_or_block" processBlock
        "parse_int_or_term" processIntOrIdentifier
  where
    processBlock _           = noop
    processIntOrIdentifier _ = noop

defineParseInteger = 
    parseAlt "parse_integer"
        "parse_integer_w8"  processInt8
        "parse_integer_w64" processInt64
  where
    processInt64 _ = callLabel "emit_ppush_w64"
    processInt8 _  = callLabel "emit_ppush_w8"
        
defineParseIntOrIdentifier = 
    parseAlt "parse_int_or_term" 
        "parse_integer"     noprocess
        "parse_identifier"  processIdentifier
  where
    noprocess _ = noop
    processIdentifier failLabel = do
        doc "parse_identifier has succeeded."
        doc "Check the kind of identifier that we have..."
        -- writeMsgHelper "Identifier case\n"

        -- writeMsgHelper "lookup identifier length:\n"
        -- callLabel "dbg_dump_ptop_w64"
        -- ppop rax
        -- callLabel "dbg_dump_ptop_w8"
        -- ppush rax

        callLabel "term_hash"
        -- writeMsgHelper "looking for identifier hash:\n"
        -- callLabel "dbg_dump_ptop_w64"

        doc "Place the hash in rax"
        ppop rax 

        doc "Now test our identifier against various options:"

        doc "Does it equal the hash of 'compile'?"
        mov rbx (I64 $ fnv1s "compiler")
        cmp rax rbx
        jneNear "parse_stmt_not_compiler"
        do 
            doc "We have a compiler statement! We wish to switch"
            doc "from emitting JIT calls to running the calls directly here."
            doc "For this, we need a state variable of some sort..."

            doc "This will be very hard to explain, but what we do is:"
            doc "(1) Generate a jump instruction to avoid the code "
            doc "that the parser generates next: TODO"

            doc "(2) we back up the current r9"
            ppush r9


            doc "(3) call the parser on a statement. This generates some "
            doc "code and increases r9. "
            callRequiredParser "parse_stmt" failLabel

            doc "(4) Update the jump instruction emitted at (1) to jump "
            doc "to the current r9 - as on an if statement that doesn't match"
            doc "TODO"

            doc "(5) Execute the code from the position we have backed up."

            ppop rax
            call rax

            doc "We have generated some junk in the definition bodies memory zone"
            doc "that we won't need anymore or reuse, but we'll find "
            doc "a better way some other time -- this is just a hack! "

            jmpLabel "parse_stmt_done"

        setLabel "parse_stmt_not_compiler"
        doc "Does it equal the hash of 'if'?"
        mov rbx (I64 $ fnv1s "if_top_nz")
        cmp rax rbx
        jneNear "parse_stmt_not_if"

        -- writeMsgHelper "If term\n"
        doc "Parsing an if term!"

        do 
            doc "Consume any whitespace after the if-term"
            callOptionalParser "parse_wss" failLabel

            callBody "emit_if_start"

            doc "Parse the if body, which could be anything from a simple"
            doc "term to a block term to another if term:"

            callRequiredParser "parse_stmt" failLabel

            doc "(Note: it is critical that the program returns here"
            doc "to the same stack point as at the emit_if_start call "
            doc "because emit_if_end uses the value placed on the stack "
            doc "by emit_if_start)!"

            callBody "emit_if_end"
            jmpLabel "parse_stmt_done"

        setLabel "parse_stmt_not_if"

        doc "Try out whether it is a return statement? (ret)"
        mov rbx (I64 $ fnv1s "ret")
        cmp rax rbx
        jneNear "parse_stmt_not_ret"

        doc "Parsing a 'ret' keyword!"
        do
            -- doc "Consume any whitespace after the ret-term"
            -- callOptionalParser "parse_wss" failLabel
            callBody "emit_ret"
            jmpLabel "parse_stmt_done"

        setLabel "parse_stmt_not_ret"

        doc "Is it an identifier found in the "
        doc "dictionary?"
        do
            -- doc "Consume any whitespace after the term"
            -- callOptionalParser "parse_wss" failLabel

            ppush rax
            callLabel "term_look"
            ppop rax
            cmp rax (I32 0)
            jeNear "parse_stmt_undefined"

            doc "Found the term in the dictionary! Emit a call to the address."
            callLabel "emit_call"

            jmpLabel "parse_stmt_done"

        setLabel "parse_stmt_undefined"
        do
            writeMsgHelper "Undefined term skipped!\n"
            doc "Note we could also have the parser ignore the failure..."
            jmpLabel failLabel

        setLabel "parse_stmt_done"


defineParseDefBodyEnd = parseSequence False "parse_def_body_end"
    noop "parse_stmt_wsss" noop "parse_dot" noop

defineParseDefBody = parseSequence True "parse_def_body"
    noop "parse_equal" noop "parse_def_body_end" noop

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

        callLabel "parse_identifier"
        ppop rax
        cmp rax $ I32 parse_reject
        jeNear rejectLabel
        cmp rax $ I32 parse_fail
        jeNear failLabel

        callLabel "new_def"

        callLabel "parse_wss" 
        pdrop 1

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
        
defineParseComment = 
    parseSequence False "parse_comment"
        noop "parse_lparen" noop "parse_comment_body" noop 

defineParseCommentBody =
    parseSequence False "parse_comment_body"
        noop "parse_norparens" noop "parse_rparen" noop 

defineParseNoRParens = 
    parseStar "parse_norparens" "parse_norparen" noop noop 

    
