{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Lang.Lang where

-- Language-specific functionality. This module sits on top of the
-- X86 monad.

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
import Lang.TestParser

import Data.Int
import Data.Word
import qualified Data.Bits as B
import Control.Monad

parse_fail         = 0 -- Parser failed and parsing should be stopped.
parse_reject       = 1 -- Parser rejected the input. Use the specific character
                       -- that was rejected with the next parser.
parse_success      = 2 -- Parser succeeded and consumed all input. 
                       -- Further input should be requested.

defineBaseDefBodies = do

    defineCMP "eq"  je
    defineCMP "lt"  jl
    defineCMP "lte" jle
    defineCMP "gt"  jg
    defineCMP "gte" jge

    defineBinop "plus"  add
    defineBinop "minus" sub
    defineBinop "times" undefined
    defineBinop "and"   and_

    defineShiftLeft
    defineShiftRight

    defineRdChrLinux
    defineWrChrLinux
    defineWrAsciiW8Linux

    defineRdHeadW8
    defineRdTailW8
    defineRdTailAZW8

    defineTermReadLinux

    defineTermHash
    defineTermLook
    defineTermLookNohash
    defineDUP
    defineDROPW64
    defineDROPW8
    defineSDROPW8
    definePPOPSPUSHW64
    defineSPOPPPUSHW64
    definePPOPSPUSHW8
    defineSPEERPPUSHW64
    definePPEERW8
    definePPEERW64
    defineNOT
    defineDEC
    defineEMITLIT
    defineEMITCALL
    defineEMITIF_START
    defineEMITIF_END
    defineEMITRET
    -- defineLIT
    -- defineIFPOP0
    -- defineIFPEEK0
    
    defineParseIdentifier
    defineParseNumeric
    defineParseDef
    defineParseIfTerms
    defineParseIfTerm

    defineParseStackParams
    defineParseStackParam
    defineParseStackRewriteTerm

    defineDbgDumpPtop64
    defineDbgDumpPtop8
    defineDbgDumpDictionary
    -- defineDbgParseTestSuite
    defineREPL
    defineREPLDef
    defineREPLDefInt

    definePush1 -- Dummy test function that just pushes constant 1
    definePushK -- Dummy test function that just pushes constant 'K'

    defineExit
    -- defineEval
    -- testSEQDefinitions

pushSimpleDef :: String -> Lang ()
pushSimpleDef defName = do
    doc $ "ASM Dictionary entry for " ++ defName
    doc "ASM or term sequence address in memory:"
    let hash = I64 $ fnv1 $ map ascii defName
    doc "Backup the old r11"
    x86 $ mov rax r11

    doc "Allocate a new record"
    x86 $ add r11 $ I32 24
    
    doc "Set values of the new record"
    x86 $ mov (derefOffset r11 0)  rax -- prev dict. entry
    x86 $ mov rax $ hash
    x86 $ mov (derefOffset r11 8)  rax -- hash
    x86 $ mov rax                 (L64 defName)
    x86 $ mov (derefOffset r11 16) rax  -- code address (by label ref.)

    {-
    x86 $ mov rax $ L64 defName
    ppush rax
    doc "Definition name hash (for easy search):"
    x86 $ mov rax $ I64 $ fnv1 $ map ascii defName
    ppush rax
    doc "Previous entry pointer:"
    ppush r11
    x86 $ mov r11 rsi
    -}

writeMsgHelper msg = do
    doc $ "Write message: " ++ msg
    ppush rax
    ppush rbx
    ppush rcx
    ppush rdx
    x86 $ do
        mov rax $ I64 $ fromIntegral $ linux_sys_write
        mov rbx $ I64 $ fromIntegral $ linux_stderr
        mov rdx $ I64 $ fromIntegral $ length msg
        asm $ addString msg
        mov rcx (S64 msg)
        int
    ppop rdx
    ppop rcx
    ppop rbx
    ppop rax

pushBaseDict = do
    doc "Keep it a linked list rather than an array because there "
    doc "might be variable-length records in the future (e.g. the "
    doc "entry name ASCII representation)"
    doc "Simple Dictionary of cells [ prev ][ name ][ addr ]"
    -- x86 $ mov r11 $ I64 0
    pushSimpleDef "repl"
    pushSimpleDef "dbg_dump_ptop_w64"
    pushSimpleDef "dbg_dump_ptop_w8"
    pushSimpleDef "dbg_dump_dictionary"
    -- pushSimpleDef "dbg_parse_test_suite"
    pushSimpleDef "read_head_w8"
    pushSimpleDef "parse_identifier"
    pushSimpleDef "parse_numeric"
    pushSimpleDef "parse_if_term"
    pushSimpleDef "parse_stack_param"
    pushSimpleDef "parse_stack_params"
    pushSimpleDef "parse_stack_rw_term"
    pushSimpleDef "not"
    pushSimpleDef "dec"
    pushSimpleDef "drop_w64"
    pushSimpleDef "drop_w8"
    pushSimpleDef "ppop_spush_w64"
    pushSimpleDef "ppop_spush_w8"
    pushSimpleDef "spop_ppush_w64"
    pushSimpleDef "speer_ppush_w64"
    pushSimpleDef "peer_w8"
    pushSimpleDef "peer_w64"
    pushSimpleDef "sdrop_w8"
    pushSimpleDef "dup"
    pushSimpleDef "eq"
    pushSimpleDef "lt"
    pushSimpleDef "lte"
    pushSimpleDef "gt"
    pushSimpleDef "gte"
    pushSimpleDef "minus"
    pushSimpleDef "times"
    pushSimpleDef "and"
    pushSimpleDef "exit"
    pushSimpleDef "plus"
    pushSimpleDef "push1"
    pushSimpleDef "pushk"
    pushSimpleDef "write_char"
    pushSimpleDef "read_char"
    pushSimpleDef "write_ascii_w8"
--    pushSimpleDef "read_printables_w8"
--    pushSimpleDef "term_look"
--    pushSimpleDef "term_look_nohash"
--    pushSimpleDef "term_hash"
    -- doc "Dictionary end marking point (LOOKUP ends here)"
    -- ppushI32 0

computeOffsets :: [(String, Type)] 
               -> [(String, Word32)]
computeOffsets xs = zipWith (\(s, t) o -> (s, o)) xs offs
  where
    sizes   = map (sizeof . snd) xs
    sumoffs = accum sizes
    t1      = head sumoffs
    offs    = map ((-) t1) sumoffs
    
accum :: Num a => [a] -> [a]
accum = a 0
  where
  a _ []     = []
  a s (x:xs) = s+x : a (s+x) xs

defineDROPW64 :: Lang () 
defineDROPW64 = defFunBasic "drop_w64" body
  where
    body    = do
        doc "Remove the top item from the stack."
        pdrop 1

defineDROPW8 :: Lang () 
defineDROPW8 = defFunBasic "drop_w8" body
  where
    body    = do
        doc "Remove the top item from the stack."
        pdropW8 1

definePPOPSPUSHW64 :: Lang ()
definePPOPSPUSHW64 = defFunBasic "ppop_spush_w64" body where
    body = do
        ppop  rax
        spush rax

defineSPOPPPUSHW64 :: Lang ()
defineSPOPPPUSHW64 = defFunBasic "spop_ppush_w64" body where
    body = do
        spop  rax
        ppush rax

definePPOPSPUSHW8 :: Lang ()
definePPOPSPUSHW8 = defFunBasic "ppop_spush_w8" body where
    body = do
        ppopW8 al
        spush  al

defineSPEERPPUSHW64 :: Lang ()
defineSPEERPPUSHW64 = defFunBasic "speer_ppush_w64" body where
    body = do
        ppop  rax -- offset
        x86 $ mov   rbx rdi
        x86 $ add   rbx rax
        x86 $ mov   rax (derefOffset rbx 0)
        ppush rax

definePPEERW8 :: Lang() 
definePPEERW8 = defFunBasic "peer_w8" body where
    body = do
        ppop rax -- offset
        x86 $ mov   rbx rsi
        x86 $ add   rbx rax
        x86 $ mov   al (derefOffset rbx 0)
        ppush al

definePPEERW64 :: Lang() 
definePPEERW64 = defFunBasic "peer_w64" body where
    body = do
        ppop rax -- offset
        x86 $ mov   rbx rsi
        x86 $ add   rbx rax
        x86 $ mov   rax (derefOffset rbx 0)
        ppush rax

defineSDROPW8 :: Lang ()
defineSDROPW8 = defFunBasic "sdrop_w8" body where
    body = do
        ppop rax -- offset
        x86 $ add rdi rax

defineDUP :: Lang () 
defineDUP = defFunBasic "dup" body
  where
    body    = do
        doc "Duplicate the top item on the stack."
        ppeek rax
        ppush rax
        x86 $ ret

defineDEC :: Lang ()
defineDEC = defFunBasic "dec" body
  where
    body    = do
        doc "Decrement the stack top."
        ppop rax
        x86 $ dec rax
        ppush rax

defineNOT :: Lang ()
defineNOT = defFunBasic funName body
  where
    funName = "not"
    ty      = undefined
    body    = do
        doc "If the top of the stack is 0, replace with 1. "
        doc "Otherwise replace with 0."
        ppop rax
        x86 $ cmp rax $ I32 0
        x86 $ jeNear "NOT_PUSH_0"
        ppush $ I32 1
        x86 $ ret
        x86 $ asm $ setLabel "NOT_PUSH_0"
        ppush $ I32 0
        x86 $ ret

defineDbgDumpDictionary :: Lang ()
defineDbgDumpDictionary = defFunBasic "dbg_dump_dictionary" body
  where
    body = do
        doc "Dump dictionary"
        doc "Backup registers"
        ppush rbx
        ppush rcx

        writeMsgHelper "Dictionary\n"
        writeMsgHelper "----------\n"
        x86 $ mov rbx r11
        x86 $ asm $ setLabel "DBG_DUMP_DICTIONARY_LOOP"
        do
            x86 $ cmp rbx $ I32 0
            x86 $ jeNear "DBG_DUMP_DICTIONARY_END"

            x86 $ mov rcx (derefOffset rbx 8)
            ppush rcx
            callBody "dbg_dump_ptop_w64"
            pdrop 1

            doc "Advance the dictionary to the following item (def.prev)"
            x86 $ mov rbx (derefOffset rbx 0) 

            x86 $ jmpLabel "DBG_DUMP_DICTIONARY_LOOP"
        setLabel "DBG_DUMP_DICTIONARY_END"

        writeMsgHelper "----------\n"
        doc "Restore register backups"
        ppop rcx
        ppop rbx

-- Same as TermLook but it doesn't hash.
defineTermLookNohash :: Lang ()
defineTermLookNohash = defFunBasic funName body
  where
    funName = "TERM_LOOK_NOHASH"
    ty      = undefined
    body    = do
        doc "Lookup a term in the dictionary."
        doc "Use the hash given on the stack."
        doc "Traverse the dictionary using rax until we find"
        doc "either the emtpy dictionary or the term."
        ppop rax 
        doc "rax holds the hash we're looking for."
        x86 $ mov rbx r11 -- Begin from the dictionary top.
        x86 $ asm $ setLabel "TERM_LOOK_WHILE"
        do
            x86 $ cmp rbx $ I32 0
            x86 $ jeNear "TERM_LOOK_NOT_FOUND"

            doc "Take the current dictionary entry hash: def.hash"
            x86 $ mov rcx (derefOffset rbx 8)
            x86 $ cmp rcx rax
            x86 $ jeNear "TERM_LOOK_FOUND"
            
            doc "Advance the dictionary to the following item (def.prev)"
            x86 $ mov rbx (derefOffset rbx 0) 

            x86 $ jmpLabel "TERM_LOOK_WHILE"
        x86 $ asm $ setLabel "TERM_LOOK_FOUND"
        doc "Found! Return the matching dictionary address"
        ppush rbx 
        ppush $ I32 1
        x86 $ ret

        x86 $ asm $ setLabel "TERM_LOOK_NOT_FOUND"
        doc "Not found! indicate that there is an error."
        ppush $ I32 0
        ppush $ I32 0
        x86 $ ret

-- Type : -> :w64:w64
-- Func : -> :term_address:success_or_failure
defineTermLook :: Lang ()
defineTermLook = defFunBasic funName body
  where
    funName = "TERM_LOOK"
    ty      = undefined
    body    = do
        doc "Lookup a term in the dictionary."
        doc "Compute the term hash."
        x86 $ callLabel "TERM_HASH"
        doc "Traverse the dictionary using rax until we find"
        doc "either the emtpy dictionary or the term."
        doc ""
        doc "rax now holds the hash we're looking for."

        x86 $ callLabel "TERM_LOOK_NOHASH"

-- Type: :w8 :..:w8   :w64 -> w64                                :w64
-- Func: :w_0:..:w_n-1:n   -> (w_n-1 + w_n-2*10 + ... w_0*10^n-1):success
-- Convert a base 10 integer string to an integer.
defineREPLDefInt :: Lang () 
defineREPLDefInt = defFunBasic "PARSEINT" body
  where
    body = do
        doc "Parse an integer that is part of the REPL definition."
        doc "rbx holds the number of string characters left"
        ppop rbx 
        doc "rcx holds a power of 10 (multiplier). It starts from number 1."
        x86 $ mov rcx $ I64 1
        doc "we store the intermediate parse result in r10"
        x86 $ xor r10 r10
        
        setLabel "REPL_DEF_INT_LOOP"

        x86 $ cmp rbx $ I32 0
        x86 $ jeNear "REPL_DEF_INT_END"

        doc "Decrease the remaining chars counter"
        x86 $ dec rbx

        -- Warning: DBG_DUMP_PTOP_8 interacts with our registers!
        -- x86 $ callLabel "DBG_DUMP_PTOP_8"

        x86 $ xor rax rax
        doc "Drop the 8-bit word from the pstack:"
        ppopW8 al

        doc "Convert from ascii to a base10 number by subtracting 0x30:"
        x86 $ sub rax $ I32 0x30
        doc "In case of overflow, return an error:"
        x86 $ joNear "REPL_DEF_INT_ERROR"

        doc "After subtraction, if rax is greater than 9, then we've had"
        doc "a non-digit character which results in a parse error:"
        x86 $ cmp rax $ I32 9
        x86 $ jgNear "REPL_DEF_INT_ERROR"

        doc "Multiply rax by the current power of 10."
        x86 $ mul rcx
        x86 $ joNear "REPL_DEF_INT_ERROR"

        doc "Add rax to the accumulator"
        x86 $ add r10 rax
        x86 $ joNear "REPL_DEF_INT_ERROR"

        doc "Multiply rcx by constant 10 (our base)"
        x86 $ mov rdx $ I64 10
        x86 $ mov rax rcx
        x86 $ mul rdx
        x86 $ joNear "REPL_DEF_INT_ERROR"
        x86 $ mov rcx rax
    
        x86 $ jmpLabel "REPL_DEF_INT_LOOP"

        setLabel "REPL_DEF_INT_END"
        ppush r10
        ppush $ I32 1
        x86 $ ret

        setLabel "REPL_DEF_INT_ERROR"
        doc "Drop any remaining W8 characters from the stack to enforce the"
        doc "function type"
        x86 $ add rsi rbx
        ppush r10
        ppush $ I32 0
        x86 $ ret

defineParseDef :: Lang () 
defineParseDef = defFunBasic "PARSE_DEF" body
  where 
    body = do
        doc "An example of input that this function can parse from"
        doc "stdin is: 'add_2 = 2 plus .'"
        doc "or with typed stack references: "
        doc "  'add_2 :w64 n = n 2 plus .'"

        callBody "READ_PRINTABLES_W8"
        ppop_assert 1 "Could not read the term to define!\n"

        callBody "TERM_HASH"
        ppop rax

        doc "Build a new [ prev ][ name hash ][ addr ] record on the stack"
        doc "for the new dictionary entry..."
        doc "In case of an error, e.g. a missing term or an integer " 
        doc "overflow, then this is rolled back in REPL_DEF_ERROR_ROLLBACK:"

        x86 $ mov rbx r11
        x86 $ add r11 $ I32 24 -- Allocate a new record
        x86 $ mov (derefOffset r11 0)  rbx -- prev dict. entry
        x86 $ mov (derefOffset r11 8)  rax -- name hash
        x86 $ mov (derefOffset r11 16) r9  -- addr

        {-
        ppush r9  -- addr
        ppush rax -- name hash 
        ppush r11 -- prev dict. entry
        
        -- x86 $ callLabel "dbg_dump_dictionary"
        doc "Set the new list pointer r11, to the stack top:"
        x86 $ mov r11 rsi
        -- x86 $ callLabel "dbg_dump_dictionary"
        -}

        {-
        x86 $ callLabel "PARSE_STACK_PARAMS"
        ppop rax
        x86 $ cmp rax $ I32 1
        x86 $ jeNear "SUCCESS_STACK_PARAMS_PARSE"

        do
            doc "Parameter parsing has failed!"
            ppush $ I32 0
            x86 $ ret

        setLabel "SUCCESS_STACK_PARAMS_PARSE"
        -}

        callBody "read_head_w8"
        ppop_assert 1 "read_head_w8 failed"

        x86 $ xor rax rax
        ppopW8 al

        doc "The ascii code for '=' is 0x3D"
        x86 $ cmp rax $ asciiI32 '='
        x86 $ jneNear "PARSE_DEF_ERROR"      

        callBody "read_head_w8"
        ppop_assert 1 "read_head_w8 failed"

        x86 $ callLabel "PARSE_TOP_TERMS"
        ppop rax
        x86 $ cmp rax $ I32 parse_fail
        x86 $ jeNear "PARSE_DEF_ERROR"

        doc "Expect a dot to be on the stack at the end of a definition:"
        x86 $ xor rax rax

        -- x86 $ callLabel "dbg_dump_ptop_w8"
        ppopW8 al
        x86 $ cmp rax $ asciiI32 '.' -- 0x2E
        x86 $ jneNear "PARSE_DEF_ERROR"
        
        ppush $ I32 1
        x86 $ ret

        setLabel "PARSE_DEF_ERROR"
        doc "Restore the previous state from the stack data:"
        x86 $ sub r11 $ I32 24 -- Dealocate the failed record

        {-
        ppop  r11 -- prev
        pdrop 1   -- name hash, don't care
        ppop  r9  -- addr
        -}

        ppush $ I32 0
        x86 $ ret

-- Parses input and leaves parameters on the stack of form
-- In  : :w8
-- Type: :w64      :w64          :w64        :w64        :w64
-- Func: :nm_hash_0:ty_hash_0:...:nm_hash_n-1:ty_hash_n-1:n  :result
defineParseStackParams :: Lang ()
defineParseStackParams = defFunBasic "parse_stack_params" body
  where
    body = do
        ppopW8 al
        doc "Count parameters using rbx, place the counter before our :w8"
        x86 $ xor rbx rbx
        -- x86 $ inc rbx
        ppush rbx
        ppush al

        doc "Call parse_stack_param in a loop."
        setLabel "PARSE_STACK_PARAMS_LOOP"
        do
            -- writeMsgHelper "First char before calling parse_stack_param:"
            callBody "parse_stack_param"

            doc "Check the success of parse_stack_param"
            ppop rax

            x86 $ cmp rax $ I32 parse_success
            x86 $ jeNear "PARSE_STACK_PARAM_SUCCESS"

            x86 $ cmp rax $ I32 parse_reject
            x86 $ jeNear "PARSE_STACK_PARAM_REJECT"

            x86 $ cmp rax $ I32 parse_fail
            x86 $ jeNear "PARSE_STACK_PARAM_FAIL"

            setLabel "PARSE_STACK_PARAM_FAIL"
            do
                doc "Parse stack_param was unsuccessful -- its requirements"
                doc "were not met. Signal general parse failure to the caller"
                doc "which should cause a parser stop."
                -- TODO: Maybe deallocate parameters previously pushed?
                -- (i.e. ensure that the stack is somehow left clean after a 
                -- failure)
                ppush $ I32 parse_fail
                x86   $ ret

            setLabel "PARSE_STACK_PARAM_REJECT"
            do
                ppush $ I32 parse_reject
                x86   $ ret

            setLabel "PARSE_STACK_PARAM_SUCCESS"
            do 
                doc "(n name type -- name type (n+1))"
                doc "On the stack we have :name_hash:type_name_hash"

                doc "Increment our rbx counter from the backup"
                doc "Remove values from the stack to get to the rbx backup"
                ppop rdx
                ppop rcx

                doc "Get the rbx backup value and get rid of it from the stack."
                ppop rbx
                x86 $ inc rbx

                doc "Put back the parse_stack_param values on the stack"
                doc "in the order we read them:"
                ppush rcx
                ppush rdx

                ppush rbx

            callBody "read_head_w8"
            ppop_assert 1 "read_head_w8 failed\n"

        x86 $ jmpLabel "PARSE_STACK_PARAMS_LOOP"


-- Type: :w8 -> :w8 :...:w8   :w64:w64
-- Func:        :w_0:...:w_n-1:n  :result = 2
-- or 
-- Type: :w8 -> :w8               :w64
-- Func:        :continuation_char:result = 0/1
-- An Identifier is a TERM that starts with ASCII chars a-z
defineParseIdentifier :: Lang ()
defineParseIdentifier = defFunBasic "parse_identifier" body
  where
    body = do
        x86 $ xor rax rax
        ppeerW8 0 al

        -- callBody "dbg_dump_ptop_w8"

        doc "Is the char less than ascii code a?"
        x86 $ cmp rax $ asciiI32 'a'
        x86 $ jlNear "PARSE_IDENTIFIER_REJECT"

        doc "Is the char greater than ascii code z?"
        x86 $ cmp rax $ asciiI32 'z'
        x86 $ jgNear "PARSE_IDENTIFIER_REJECT"

        doc "Parsing a sequence of a-z,0-9 and _."
        doc "Place the rest of the input onto the stack..."
        x86 $ callLabel "READ_TAIL_AZ_W8"
        ppop rax

        x86 $ cmp rax $ I32 parse_success
        x86 $ jeNear "PARSE_IDENTIFIER_SUCCESS"

        x86 $ cmp rax $ I32 parse_reject
        x86 $ jeNear "PARSE_IDENTIFIER_SUCCESS"

        doc "Failure.."
        ppush $ I32 parse_fail
        x86 $ ret

        setLabel "PARSE_IDENTIFIER_SUCCESS"
        do 
            doc "Increment the count to account for the first character."
            ppopW8 al -- continuation character
            ppop rbx
            x86 $ inc rbx
            ppush rbx
            ppush al

            ppush $ I32 parse_success
            x86 $ ret

        setLabel "PARSE_IDENTIFIER_REJECT"
        ppush $ I32 parse_reject
        x86 $ ret

defineParseNumeric = defFunBasic "parse_numeric" body
  where
    body = do
        x86 $ xor rax rax
        ppeerW8 0 al

        doc "Is the char greater than ascii code 9?"
        x86 $ cmp rax $ I32 0x39
        x86 $ jgNear "PARSE_NUMERIC_REJECT"

        doc "Is the char less than ascii code 0?"
        x86 $ cmp rax $ I32 0x30 
        x86 $ jlNear "PARSE_NUMERIC_REJECT"

        doc "Our input starts with 0-9. Therefore, "
        doc "attempt to parse a positive integer literal."
        doc "Read the rest of the input, until whitespace ..."
        x86 $ callLabel "READ_TAIL_W8"
        assertPtop 1 "READ_TAIL_W8 was not successful!"
        pdrop 1

        ppop rbx
        x86 $ inc rbx
        ppush rbx

        x86 $ callLabel "PARSEINT"
        ppop rax
        x86 $ cmp rax $ I32 1
        x86 $ jneNear "PARSE_NUMERIC_ERROR"

        x86 $ callLabel "read_head_w8"
        ppop_assert 1 "L.706 read_head_w8"

        ppush $ I32 parse_success -- success
        x86 $ ret

        setLabel "PARSE_NUMERIC_REJECT"
        ppush $ I32 parse_reject -- non-fatal parse failure, continue parsing.
        x86 $ ret

        setLabel "PARSE_NUMERIC_ERROR"
        ppush $ I32 parse_fail -- fatal parse failure, stop parsing.
        x86 $ ret


-- Input:   :w8
-- Returns: :w64      :w64           :w64
-- Func:    :name_hash:type_name_hash:result=2
-- OR
-- Returns: :w8             :w64
-- Func:    :parse_cont_char:result=0/1
--
-- parse_cont_char: When match = 0, this is the lookup character that
-- made me fail...
-- match: The parser matched the input initially
-- success: All parser input requirements were fulfilled
defineParseStackParam :: Lang ()
defineParseStackParam = defFunBasic "parse_stack_param" body
  where
    body = do
        {-
        x86 $ callLabel "read_head_w8"
        assertPtop 1 "read_head_w8 failed\n"
        pdrop 1
        -}
        -- callBody "dbg_dump_ptop_w8"

        x86 $ xor rax rax
        ppopW8 al

        doc "A parameter is preceded by a colon, which signifies a stack "
        doc "cell (inspired by Haskell's cons operator)."
        doc "0x3A is ASCII ':' "
        x86 $ cmp rax $ asciiI32 ':'
        x86 $ jeNear "IS_STACK_PARAM"

        do
            ppush $ al               -- Continuation character
            ppush $ I32 parse_reject -- the input doesn't match this parser
            x86 $ ret

        setLabel "IS_STACK_PARAM"

        doc "Parse the type name:"
        do
            callBody "read_head_w8"
            assertPtop 1 "read_head_w8 failed\n"
            pdrop 1
            callBody "parse_identifier"
            ppop rax
            x86 $ cmp rax $ I32 parse_success
            x86 $ jeNear "STACK_PARAM_HAS_TYPE"

            doc "We've encountered a non-identifier where"
            doc "one is requried."
            doc "This should be a stopping error for the parser."
            do
                ppush $ I32 parse_fail -- fail (critical).
                x86 $ ret

            setLabel "STACK_PARAM_HAS_TYPE"

            x86 $ callLabel "TERM_HASH"

        doc "Now that the type name hash is on the stack, the"
        doc "parameter identifier follows:"

        callBody "read_head_w8"
        assertPtop 1 "read_head_w8 failed\n"
        pdrop 1
        callBody "parse_identifier"
        ppop rax
        x86 $ cmp rax $ I32 parse_success
        x86 $ jeNear "HAS_PARAM_NAME"

        do 
            ppush $ I32 parse_fail -- critical failure
            x86   $ ret

        setLabel "HAS_PARAM_NAME"
        x86 $ callLabel "TERM_HASH"

        ppush $ I32 parse_success 
        x86   $ ret

defineParseStackRewriteTerm :: Lang ()
defineParseStackRewriteTerm = defFunBasic "parse_stack_rw_term" body
  where
    body = do
        doc "Try to parse a stack-rewrite command. If that fails then reject."

        x86 $ xor rax rax
        ppeerW8 0 al

        x86 $ cmp rax $ asciiI32 '('
        x86 $ jeNear "parse_stack_rw_start_success"
        do
            doc "The term doesn't start with a '(' so it's not a rw term."
            ppush $ I32 parse_reject
            x86 $ ret

        setLabel "parse_stack_rw_start_success"

        doc "Consume the '('"
        ppopW8 al

        doc "On the LHS there must be at least one parameter, and hence"
        doc "we parse it:"

        doc "Await a new char..."
        callBody "read_head_w8"
        ppop_assert 1 "L.810 read_head_w8 failure...\n"

        callBody "parse_stack_param"

        ppop rax
        x86 $ cmp rax $ I32 parse_success
        x86 $ jeNear "parse_stack_from_first_success"
        do
            doc "Parameter parsing was rejected or failed, fail"
            ppush $ I32 parse_fail
            x86 $ ret
            {- doc "Parameter parsing was rejected, continue with the IF "
            doc "term parser."
            callBody "parse_if_term"
            x86 $ ret
            -}

        setLabel "parse_stack_from_first_success"

        doc "Parse the rest of the parameters (which might be absent!)"
        doc "parse_stack_params can only ever return a reject or a fail."
        doc "The reason is that the only way for it to stop is to encounter "
        doc "something that isn't a parameter of form ':type value', which "
        doc "makes the parser issue a reject."

        callBody "read_head_w8"
        ppop_assert 1 "L.836 read_head_w8 failure...\n"

        callBody "parse_stack_params"
        ppop rax
        -- assert rax != parse_success
        x86 $ cmp rax $ I32 parse_reject
        x86 $ jeNear "success_parse_stack_from_rest"
        do
            doc "Parameter parsing has failed in some way - fail."
            ppush $ I32 parse_fail
            x86 $ ret

        setLabel "success_parse_stack_from_rest"

        -- TODO: Check that there is at least one term on the left
        -- hand side. If there isn't, it's a reject.

        x86 $ xor rax rax
        ppopW8 al
        x86 $ cmp rax $ asciiI32 '-'
        x86 $ jeNear "success_parse_rw_dash"
        do
            doc "Parameter parsing has failed, there should be a dash."
            writeMsgHelper "Stack RW term parse failed, missing '-' char!\n"
            ppush $ I32 parse_fail
            x86 $ ret

        setLabel "success_parse_rw_dash"

        callBody "read_head_w8"
        ppop_assert 1 "L.866 read_head_w8 failure...\n"

        callBody "parse_stack_params"

        ppop rax
        x86 $ cmp rax $ I32 parse_reject
        x86 $ jeNear "done_parse_stack_to"
        do
            writeMsgHelper "Parameter parsing has failed, abort!\n"
            doc "Parameter parsing has failed, abort."
            ppush $ I32 parse_fail
            x86 $ ret

        setLabel "done_parse_stack_to"
        doc "Successfully parsed 'from - to' sequence. Finally we expect a ')'"

        -- callBody "read_head_w8"
        -- ppop_assert 1 "L.882 read_head_w8 failure...\n" 

        x86 $ xor rax rax
        ppopW8 al

        x86 $ cmp rax $ asciiI32 ')'
        x86 $ jeNear "parse_stack_rw_end_success"
        do
            doc "The term doesn't end with a ')' so it's not a rw term."
            ppush $ I32 parse_fail
            x86 $ ret

        setLabel "parse_stack_rw_end_success"

        doc "Consume the ')'"
        ppush $ I32 parse_success
        x86 $ ret


defineParseIfTerms :: Lang ()
defineParseIfTerms = defFunBasic "PARSE_TOP_TERMS" body
  where
    body = do
        doc "Call PARSE_IF_TERM in a loop"
        setLabel "parse_top_terms_loop"

        {- TODO: Fix to new convention
        x86 $ callLabel "parse_stack_rw_term"
        ppop rax
        x86 $ cmp rax $ I32 parse_fail
        x86 $ jeNear "parse_top_terms_fail"
        x86 $ cmp rax $ I32 parse_reject
        x86 $ jeNear "parse_stack_rw_term_reject"

        doc "successfully parsed a rw_term!"
        callBody "read_head_w8"
        ppop_assert 1 "L.918 read_head_w8 failure...\n"

        x86 $ jeNear "parse_top_terms_loop"

        -- writeMsgHelper "Char on the stack:\n"
        -- callBody "dbg_dump_ptop_w8"

        setLabel "parse_stack_rw_term_reject"
        -}

        callBody "parse_if_term"
        -- Check for success
        ppop rax
        x86 $ cmp rax $ I32 parse_fail
        x86 $ jeNear "parse_top_terms_fail"
        x86 $ cmp rax $ I32 parse_reject
        x86 $ jeNear "parse_top_terms_reject"

        -- We don't need to read a char, because the parser returns that now.
        -- doc "We read a char to feed the next parse_if_term call."
        -- callBody "read_head_w8"
        -- ppop_assert 1 "L.925 read_head_w8 failure...\n"

        x86 $ jmpLabel "parse_top_terms_loop"

        setLabel "parse_top_terms_reject"
        ppush $ I32 parse_reject
        x86 $ ret

        setLabel "parse_top_terms_fail"
        ppush $ I32 parse_fail
        x86 $ ret

defineParseIfTerm :: Lang ()
defineParseIfTerm = defFunBasic "parse_if_term" body
  where
    body = do
        doc "Parse a term (an IF term or a simpler one)."

        {-
        x86 $ callLabel "read_head_w8"
        assertPtop 1 "read_head_w8 failed\n"
        pdrop 1
        -}

        doc "Patern match on the first character that was read "
        doc "i.e. the stack top / the result of read_head_w8 / rax:"

        callBody "parse_numeric"
        doc "Interpret the result of parse_numeric:"
        ppop rax
        x86 $ cmp rax $ I32 parse_success
        x86 $ jeNear "PARSE_IF_TERM_IS_NUMERIC"
        x86 $ cmp rax $ I32 parse_reject
        x86 $ jeNear "PARSE_IF_TERM_NOT_NUMERIC"
        x86 $ cmp rax $ I32 parse_fail
        x86 $ jeNear "PARSE_IF_TERM_INT_ERROR"

        setLabel "PARSE_IF_TERM_IS_NUMERIC"
        do
            doc "Action on success:"
            -- Backup the continuation character
            ppop  al
            spush al
            x86 $ callLabel "EMIT_LIT_64"

            doc "Parse successful."
            -- Restore the continuation character
            spop  al
            ppush al
            ppush $ I32 parse_success
            x86 $ ret

        setLabel "PARSE_IF_TERM_NOT_NUMERIC"

        callBody "parse_identifier"

        doc "Interpret the result of parse_identifier:"
        ppop rax
        x86 $ cmp rax $ I32 parse_success
        x86 $ jeNear "PARSE_IF_TERM_IS_IDENTIFIER"
        x86 $ cmp rax $ I32 parse_reject
        x86 $ jeNear "PARSE_IF_TERM_REJECT_INPUT"
        -- x86 $ cmp rax $ I32 parse_fail
        -- x86 $ jeNear "PARSE_IF_TERM_IDENTIFIER_ERROR"
        
        setLabel "PARSE_IF_TERM_REJECT_INPUT"
        doc "We've reached an unknwon ascii character (e.g. a dot)"
        doc "We stop parsing and signal that we don't know how to "
        doc "handle this"
        ppush $ I32 parse_reject
        x86 $ ret

        setLabel "PARSE_IF_TERM_IS_IDENTIFIER"
        do
            -- Pop the parser continuation character because we wish to
            -- pass it on to next parser!
            x86 $ xor rdx rdx
            ppop  bl
            spush bl -- Write the cont. char to the scratch stack (easier)

            callBody "TERM_HASH"
            ppop rax
            spop  bl
            ppush bl -- Put back cont. char

            ppush rax -- hash

            x86 $ mov rbx (I64 $ fnv1s "if")
            x86 $ cmp rax rbx
            x86 $ jneNear "PARSE_IF_TERM_NOT_IF"

            pdrop 1 -- Cleanup: remove the term.
            doc "Parsing an IF. After it there should be a '{'"
            
            -- callBody "read_head_w8"
            -- ppop_assert 1 "read_head_w8 failed\n"
            -- ppush dl

            x86 $ xor rax rax
            ppopW8 al
            x86 $ cmp rax $ asciiI32 '{'
            x86 $ jneNear "PARSE_IF_TERM_NO_CURLY_BRACE_OPEN"

            doc "Emit the 'IF' initial part"
            callBody "EMIT_IF_START"

            callBody "read_head_w8"
            ppop_assert 1 "read_head_w8 failed"

            x86 $ callLabel "PARSE_TOP_TERMS"
            ppop rax
            x86 $ do
                cmp rax $ I32 parse_reject
                jeNear "PARSE_TOP_TERMS_CONTINUE"
                cmp rax $ I32 parse_fail
                jeNear "PARSE_TOP_TERMS_FAIL"

            setLabel "PARSE_TOP_TERMS_FAIL"

            -- Consume all input until there is a '}'? 
            -- Not recommended, because we might not have one.
            -- This is kind of tricky.

            writeMsgHelper "'if' body parse failure.\n"
            ppush $ I32 parse_fail
            x86 $ ret
        
            setLabel "PARSE_TOP_TERMS_CONTINUE"
            doc "After PARSE_TOP_TERMS returns there should be a '}'"
            x86 $ xor rax rax
            ppopW8 al
            x86 $ cmp rax $ asciiI32 '}'
            x86 $ jneNear "PARSE_IF_TERM_NO_CURLY_BRACE_CLOSE"

            doc "Complete the 'IF'!"
            callBody "EMIT_IF_END"
            
            x86 $ callLabel "read_head_w8"
            ppop_assert 1 "read_head_w8 failed"

            ppush $ I32 parse_success
            x86   $ ret

        setLabel "PARSE_IF_TERM_NOT_IF"
        do
            doc "Is it a RETURN statement?"
            ppeek rax

            x86 $ mov rbx (I64 $ fnv1s "return")
            x86 $ cmp rax rbx
            x86 $ jneNear "PARSE_IF_TERM_NOT_RETURN"

            pdrop 1 -- Cleanup: remove the term.
            doc "Yes, it's a RETURN statement."
            x86 $ callLabel "EMIT_RET"

            ppush $ I32 parse_success
            x86 $ ret

        setLabel "PARSE_IF_TERM_NOT_RETURN"
        {-
        do 
            doc "Try to see if the value isn't a named stack reference"
            doc "of the current definition..."
            doc "Our stack at this point holds the number of parameters!"
            doc "and the parameters returned by PARSE_STACK_PARAMS (which "
            doc "might be 0)"

            writeMsgHelper  "Check stack reference for:\n"
            x86 $ callLabel "dbg_dump_ptop_w64"
            pdrop 1
            {-
            x86 $ callLabel "dbg_dump_ptop_w64"
            pdrop 1
            x86 $ callLabel "dbg_dump_ptop_w64"
            pdrop 1
            x86 $ callLabel "dbg_dump_ptop_w64"
            pdrop 1
            x86 $ callLabel "dbg_dump_ptop_w64"
            pdrop 1
            x86 $ callLabel "dbg_dump_ptop_w64"
            pdrop 1
            x86 $ callLabel "dbg_dump_ptop_w64"
            pdrop 1
            -}
            x86 $ ret

        setLabel "PARSE_IF_TERM_NOT_PARAMETER"
        -}
        do
            -- x86 $ callLabel "dbg_dump_ptop_w64" -- hash
            -- x86 $ callLabel "dbg_dump_dictionary"


            doc "Parse a term other than IF"
            x86 $ callLabel "TERM_LOOK_NOHASH"

            doc "Check for an unknown term (do nothing in that case)."
            ppop rax
            x86 $ cmp rax $ I32 0
            x86 $ jeNear "PARSE_IF_TERM_UNDEFINED"
            
            doc "Successful term read; emit a call command to the"
            doc "term address:"
            x86 $ callLabel "EMIT_CALL"

            ppush $ I32 parse_success
            x86 $ ret

        setLabel "PARSE_IF_TERM_NOT_AZ"
        ppush $ I32 parse_fail
        x86 $ ret

        setLabel "PARSE_IF_TERM_UNDEFINED"
        writeMsgHelper "Undefined term!\n"
        ppush $ I32 parse_fail
        x86 $ ret

        setLabel "PARSE_IF_TERM_NO_CURLY_BRACE_CLOSE"
        writeMsgHelper "'if' missing '}'\n"
        ppush $ I32 parse_fail
        x86 $ ret

        setLabel "PARSE_IF_TERM_NO_CURLY_BRACE_OPEN"
        writeMsgHelper "'if' missing '{'!\n"
        ppush $ I32 parse_fail
        x86 $ ret

        {-
        setLabel "PARSE_IF_TERM_IDENTIFIER_ERROR"
        pdrop 1
        writeMsgHelper "Identifier parse error!\n"
        ppush $ I32 parse_fail
        x86 $ ret
        -}

        setLabel "PARSE_IF_TERM_INT_ERROR"
        pdrop 1
        writeMsgHelper "Integer parse error!\n"
        ppush $ I32 parse_fail
        x86 $ ret
        
defineREPLDef :: Lang ()
defineREPLDef = defFunBasic "REPL_DEF" body
  where
    body = do
        doc "This is the REPL functionality that parses new definitions."
        doc "It is not part of the dictionary, it's"
        doc "only a procedure called from the REPL function when the "
        doc "'def' command is issued."

        callBody "PARSE_DEF"
        ppop rax
        x86 $ cmp rax $ I32 0
        x86 $ jeNear "REPL_DEF_FAIL"

        x86 $ callLabel "EMIT_RET"
        writeMsgHelper  "Definition added.\n"
        x86 $ ret

        setLabel "REPL_DEF_FAIL"
        writeMsgHelper "Definition failed!\n"
        x86 $ ret

defineREPL :: Lang ()
defineREPL = defFunBasic "repl" body
  where
    body = do
        doc "REPL"
        x86 $ asm $ setLabel "REPL_START"

        doc "Read a term:"
        x86 $ callLabel "READ_PRINTABLES_W8"
        assertPtop 1 "READ_PRINTABLES_W8 returned an error!"
        pdrop 1
        doc "Hash the term we've read:"
        x86 $ callLabel "TERM_HASH"

        -- The first word is the operation we wish to make.
        -- There are two operations for now, define and call.
        doc "Switch on a different functionality"
        doc "depending on the hash of the term we've just read:"

        ppop rax
        x86 $ mov rbx (I64 $ fnv1s "def")
        x86 $ cmp rax rbx
        x86 $ jeNear "REPL_DEF_CALL"
        x86 $ mov rbx (I64 $ fnv1s "run")
        x86 $ cmp rax rbx
        x86 $ jeNear "REPL_RUN"
        x86 $ mov rbx (I64 $ fnv1s "q")
        x86 $ cmp rax rbx
        x86 $ jeNear "REPL_QUIT"
        
        writeMsgHelper "Unknown command! (expected: def/run/q)\n"
        x86 $ jmpLabel "REPL_START"

        x86 $ asm $ setLabel "REPL_DEF_CALL"
        x86 $ callLabel "REPL_DEF"
        x86 $ jmpLabel "REPL_START" -- After the definition, resume from the 
                                    -- beginning.
        x86 $ asm $ setLabel "REPL_RUN"
        x86 $ callLabel "READ_PRINTABLES_W8"
        assertPtop 1 "Could not read definition term"
        pdrop 1
        x86 $ callLabel "TERM_LOOK"
        -- Check for an unknown term (do nothing in that case).
        ppop rax
        x86 $ cmp rax (I32 0)
        x86 $ jeNear "REPL_RUN_UNDEFINED"
        
        -- Now take the '.addr' field from the dictionary term found by LOOK:
        ppop rax
        x86 $ mov rax (derefOffset rax 16)
        x86 $ call rax

        writeMsgHelper "Run term done.\n"
        x86 $ jmpLabel "REPL_START"

        x86 $ asm $ setLabel "REPL_RUN_UNDEFINED"
        assertPtop 0 "TERM_LOOK failed so the result should be 0."
        pdrop 1 -- Drop the 0 From TERM_LOOK
        writeMsgHelper "L.1158 Undefined term!\n"
        x86 $ jmpLabel "REPL_START"

        x86 $ asm $ setLabel "REPL_QUIT"
        

-- Emits the current position and pushes on the stack
-- the address at which the 32-bit offset to be later on filled
-- is located.
defineEMITIF_START :: Lang () 
defineEMITIF_START = defFunBasic "EMIT_IF_START" body
  where
    body = do
        doc "Emit IF start"
        -- ppop rax
        -- 48 8B 06 : mov rax,[rsi]
        x86 $ do
            mov (derefOffset r9 0) (I8 0x48)
            mov (derefOffset r9 1) (I8 0x8B)
            mov (derefOffset r9 2) (I8 0x06)
            add r9 (I32 3)

        -- 48 3D 00 00 00 00 : cmp rax,0x0
        x86 $ do
            mov (derefOffset r9 0) (I8 0x48)
            mov (derefOffset r9 1) (I8 0x3D)
            mov (derefOffset r9 2) (I8 0x00)
            mov (derefOffset r9 3) (I8 0x00)
            mov (derefOffset r9 4) (I8 0x00)
            mov (derefOffset r9 5) (I8 0x00)
            add r9 (I32 6)

        -- 0x0F 0x84           -- JE NEAR opcode
        -- 0x00 0x00 0x00 0x00 -- 32-bit offset, to be filled afterwards.
        doc "We don't yet know the jump offset. To fill this"
        doc "out later (in EMIT_IF_END), save the position at which the "
        doc "address is to be written."
        doc "The offset is to be calculated from the position of JE (0F 84)"
        x86 $ do
            mov (derefOffset r9 0) (I8 0x0F)
            mov (derefOffset r9 1) (I8 0x84)
            mov (derefOffset r9 2) (I8 0x00)
            mov (derefOffset r9 3) (I8 0x00)
            mov (derefOffset r9 4) (I8 0x00)
            mov (derefOffset r9 5) (I8 0x00)
            add r9 (I32 6)

        ppush r9

-- Complete the definition created by defineEMITIF_START
defineEMITIF_END :: Lang () 
defineEMITIF_END = defFunBasic "EMIT_IF_END" body
  where
    body = do
        doc "Emit IF end (it doesn't emit opcode, it just writes the current"
        doc "r9)"

        ppop rax              -- Saved R9
                              -- rbx := Offset =
        x86 $ mov rbx r9        -- Current offset R9
        x86 $ sub rbx rax       -- Minus saved offset R9
        
        -- TODO: simplify - copy from 32 bit register eax 4 bytes in 1 shot
        x86 $ do
            mov (derefOffset rax (-4)) bl
            sar rbx (I8 8)
            mov (derefOffset rax (-2)) bl
            sar rbx (I8 8)
            mov (derefOffset rax (-3)) bl
            sar rbx (I8 8)
            mov (derefOffset rax (-1)) bl

       
-- (w64 dict_address:w64 n -- :)
defineEMITCALL :: Lang () 
defineEMITCALL = defFunBasic "EMIT_CALL" body
  where
    body = do
        doc "Emit a call to a certain dictionary entry, the address of which"
        doc "is on the stack."

        ppop rax
        x86 $ mov rax (derefOffset rax 16)

        {-
        -- CALL using E8 WARNING! THis uses relative addresses!
        -- It's trickier (but we will have to optimize this probably)
        -- https://stackoverflow.com/questions/19552158
        -- rax = 00000000C00008EB
        x86 $ do
            mov (derefOffset r9 0) (I8 0xE8)
            mov (derefOffset r9 1) al
            sar rax (I8 8)
            mov (derefOffset r9 2) al
            sar rax (I8 8)
            mov (derefOffset r9 3) al
            sar rax (I8 8)
            mov (derefOffset r9 4) al
            add r9 (I32 5)
        -}
        -- mov rax <addr>
        x86 $ do
            mov (derefOffset r9 0) (I8 0x48)
            mov (derefOffset r9 1) (I8 0xB8)
            mov (derefOffset r9 2) rax
            add r9 (I32 10)

        -- call rax
        x86 $ do
            mov (derefOffset r9 0) (I8 0xFF)
            mov (derefOffset r9 1) (I8 0xD0)
            add r9 (I32 2)
            
defineEMITRET :: Lang ()
defineEMITRET = defFunBasic "EMIT_RET" body 
  where
    body = do
        doc "Emit a return statement which should be always present at "
        doc "the end of an assembly body definition."
        doc "X86: ret"
        x86 $ do
            mov (derefOffset r9 0) (I8 0xC3)
            inc r9
           
defineEMITLIT :: Lang ()
defineEMITLIT = defFunBasic funName body
  where
    funName = "EMIT_LIT_64"
    ty      = undefined
    body    = do
        doc "Emit assembly code in the dynamic code area"
        doc "That pushes a 64bit literal value on the stack."
        x86 $ xor rax rax
        ppop rax -- Take the literal that we need to push and place it in rax

        -- TODO: Improve code (repeated code)
        doc "X86: sub RSI 8"
        x86 $ do
            mov (derefOffset r9 0) (I8 0x48)
            mov (derefOffset r9 1) (I8 0x81)
            mov (derefOffset r9 2) (I8 0xEE)
            mov (derefOffset r9 3) (I8 0x08)
            mov (derefOffset r9 4) (I8 0x00)
            mov (derefOffset r9 5) (I8 0x00)
            mov (derefOffset r9 6) (I8 0x00)
            add r9 (I32 7)

        doc "X86: mov [RSI+0] <- IMM32"
        x86 $ do
            mov (derefOffset r9 0) (I8 0x48)
            mov (derefOffset r9 1) (I8 0xC7)
            mov (derefOffset r9 2) (I8 0x46)
            mov (derefOffset r9 3) (I8 0x00)
            -- TODO: simplify - copy from 32 bit register eax 4 bytes directly
            mov (derefOffset r9 4) al
            sar rax (I8 8)
            mov (derefOffset r9 5) al
            sar rax (I8 8)
            mov (derefOffset r9 6) al
            sar rax (I8 8)
            mov (derefOffset r9 7) al
            add r9 (I32 8)

        
{-
definePTopWrite :: Lang ()
definePTopWrite = defFunBasic funName body
  where
    funName = "PTOP_WRITE"
    ty = undefined
    body = do
        doc "Prints the top 64 bit word from the top of the stack as"
        doc "a HEX value on screen."
        ppop rax
        x86 $ mov rbx 8 -- There are 8 chars to write

        setLabel "PTOP_LOOP_START"
        x86 $ cmp rbx 0
        x86 $ je "PTOP_WRITE_DONE"

        x86 $ dec rbx
        -- Depending on the value of I8, print an ascii char.
        
        x86 $ sar rax (I8 1)

        x86 $ jmpLabel "PTOP_LOOP_START"

        x86 $ asm $ setLabel "PTOP_WRITE_DONE"
        x86 $ ret
-}
        

-- Improve: Is TESH_HASH expressible as a sequence of more basic words?
-- ( c0..cn-1:n -- hash )
-- alters: rbx, rcx, rax, rdx
defineTermHash :: Lang ()
defineTermHash = defFunBasic "TERM_HASH" body
  where
    body    = do
        doc "Given a word on the stack, compute its hash. This helps to"
        doc "compare words for equality when searching the dictionary."
        doc "Trying out the (simple) FNV Hash function from Wikipedia"
        doc "RBX holds the length to iterate."
        doc "E.g. 'cba' has hash 15626587013303479755"
        ppop rbx
        doc "RAX holds the hash."
        x86 $ mov rax $ I64 fnvOffsetBasis

        x86 $ asm $ setLabel "TERM_HASH_WHILE"
        do 
            x86 $ cmp rbx $ I32 0
            x86 $ jeNear "TERM_HASH_BREAK"
            x86 $ mov rcx (I64 fnvPrime)
            x86 $ mul rcx
            x86 $ xor rcx rcx -- Zeroing rcx is necessary
            ppopW8 cl
            x86 $ xor rax rcx
            x86 $ dec rbx
            x86 $ jmpLabel "TERM_HASH_WHILE"
        x86 $ asm $ setLabel "TERM_HASH_BREAK"

        ppush rax
        x86 $ ret

defineConsumeWhitespace :: Lang ()
defineConsumeWhitespace = defFunBasic funName body
  where
    funName = ""
    ty      = undefined
    body    = do
        doc "Consume input characters "

-- Type: -> w8 :...:w8   :w64:w64
-- Func: -> w_0:...:w_n-1:n  :success
-- Pushes a sequence of printable characters that come from stdin
defineTermReadLinux :: Lang ()
defineTermReadLinux = defFunBasic funName body
  where
    funName = "READ_PRINTABLES_W8"
    ty      = undefined -- Unexpressible atm.
    body    = do
        doc "read_head_w8 consumes any whitespace present before"
        doc "the term and it pushes the first encountered non-ws char."
        callBody "read_head_w8"
        assertPtop 1 "read_head_w8 failed\n"
        pdrop 1

        doc "Reads characters until a space or a control "
        doc "char (non-printable) is read."
        x86 $ callLabel "READ_TAIL_W8"
        ppop rax
        ppop rbx

        doc "Increment the number of characters reutnred by "
        doc "READ_TAIL_W8 to account for the extra char on "
        doc "the pstack placed by read_head_w8 above."
        x86 $ inc rbx
        ppush rbx
        ppush rax

-- Parses input. Skips whitespace, control characters, etc. 
-- Stops at the first non-such char and pushes it on the stack.
-- Type : -> :w8        :w64
-- Func : -> :read_char:success
defineRdHeadW8 :: Lang ()
defineRdHeadW8 = defFunBasic "read_head_w8" body
  where
    body = do
        doc "Allocate 1 byte on the parameter-stack in which our"
        doc "first non-whitespace character is placed:"
        ppush $ I8 0x00

        x86 $ asm $ setLabel "RPC_WHILE"
        doc "Please read"
        x86 $ mov rax $ I64 $ fromIntegral linux_sys_read
        doc "1 char"
        x86 $ mov rdx $ I64 0x01 
        doc "From stdin"
        x86 $ xor rbx rbx
        doc "Into the pstack"
        x86 $ mov rcx rsi
        x86 $ int

        x86 $ cmp rax $ I32 0x00
        x86 $ jeNear "RPC_ERROR"

        x86 $ xor rax rax
        ptopW8 al

        x86 $ cmp rax $ I32 0x20 
        x86 $ jleNear "RPC_WHILE" -- Space or Control character, skip
        x86 $ cmp rax $ I32 0x7F
        x86 $ jeNear  "RPC_WHILE" -- ESC, skip

        ppush $ I32 1 -- Success
        x86 $ ret

        x86 $ asm $ setLabel "RPC_ERROR"
        ppush $ I32 0 -- Fail

defineRdTailAZW8 :: Lang ()
defineRdTailAZW8 = defFunBasic "READ_TAIL_AZ_W8" body
  where
    body = do
        doc "READ_TAIL_AZ_W8 reads characters until a non[a-z,0-9,_]"
        doc "character is encountered."
        doc "It pushes on the stack an array of w8s then it pushes a"
        doc "w64 indicating how many chars have been read,"
        doc "followed by a w64 indicating success or failure."
        doc "r15 counts the chars that are successfully read."
        x86 $ xor r15 r15
        x86 $ asm $ setLabel "READ_TAIL_AZ_WHILE"

        doc "Allocate 1 byte on the stack to read into"
        ppush $ I8 0x00
        doc "Please read"
        x86 $ mov rax $ I64 $ fromIntegral linux_sys_read
        doc "1 char"
        x86 $ mov rdx $ I64 0x01 
        doc "From stdin"
        x86 $ xor rbx rbx
        doc "Into the pstack"
        x86 $ mov rcx rsi
        x86 $ int

        x86 $ cmp rax $ I32 0x00
        x86 $ jeNear "READ_TAIL_AZ_W8_ERROR"
       
        doc "Place the char we've just read into rax"
        x86 $ xor rax rax
        ptopW8 al

        doc "Probe the char we just read if it's non-a-z "
        doc "then break the loop:"
        x86 $ cmp rax $ asciiI32 'a'
        x86 $ jlNear "READ_TAIL_AZ_W8_NON_AZ" 
        x86 $ cmp rax $ asciiI32 'z'
        x86 $ jgNear "READ_TAIL_AZ_W8_NON_AZ"

        doc "It's an a-z. Repeat"
        x86 $ inc r15
        x86 $ jmpLabel "READ_TAIL_AZ_WHILE"

        setLabel "READ_TAIL_AZ_W8_NON_AZ"
        x86 $ cmp rax $ asciiI32 '0'
        x86 $ jlNear "READ_TAIL_AZ_W8_NON_09" 
        x86 $ cmp rax $ asciiI32 '9'
        x86 $ jgNear "READ_TAIL_AZ_W8_NON_09"

        doc "It's a 0-9. Repeat"
        x86 $ inc r15
        x86 $ jmpLabel "READ_TAIL_AZ_WHILE"
       
        setLabel "READ_TAIL_AZ_W8_NON_09"
        x86 $ cmp rax $ asciiI32 '_'
        x86 $ jneNear "READ_TAIL_AZ_W8_NON__"

        doc "It's an underscore. Repeat"
        x86 $ inc r15
        x86 $ jmpLabel "READ_TAIL_AZ_WHILE"

        setLabel "READ_TAIL_AZ_W8_NON__"

        doc "Check whether it's a non-printable character."
        doc "If so then call read_head_w8 and end with success."

        x86 $ cmp rax $ asciiI32 ' ' -- Space or less, skip
        x86 $ jleNear "READ_TAIL_W8_IDENTIFIER_SUCCESS" 
        x86 $ cmp rax $ I32 0x7F -- ESC
        x86 $ jeNear  "READ_TAIL_W8_IDENTIFIER_SUCCESS"

        doc "We've reached a non-acceptable, non-space char, but we "
        doc "have a result. Return with that character and success."

        ppopW8 al
        doc "Number of chars read:"
        ppush $ r15
        doc "Continuation character for the next parser:"
        ppush al
        ppush $ I32 parse_success
        x86 $ ret

        setLabel "READ_TAIL_W8_IDENTIFIER_SUCCESS"
        doc "Drop the whitespace char and succeed."
        pdropW8 1

        doc "Number of chars read (part of the data):"
        ppush $ r15

        doc "Call read_head_w8 to obtain a valid continuation character."
        x86 $ callLabel "read_head_w8"
        ppop_assert 1 "L.1641 could not read_head_w8!\n"

        doc "Success:"
        ppush $ I32 parse_success
        x86 $ ret

        setLabel "READ_TAIL_AZ_W8_ERROR"
        doc "Free the stack from the last allocation:"
        ppopW8 al
        doc "Error:"
        ppush $ I32 parse_fail
        x86 $ ret

defineRdTailW8 :: Lang ()
defineRdTailW8 = defFunBasic "READ_TAIL_W8" body
  where
    body = do
        doc "READ_TAIL_W8 reads characters until a non-printable"
        doc "character is encountered."
        doc "It pushes on the stack an array of w8s then it pushes a"
        doc "w64 indicating how many chars have been read,"
        doc "followed by a w64 indicating success or failure."
        doc "r15 counts the chars that are successfully read."
        x86 $ xor r15 r15
        x86 $ asm $ setLabel "READ_TAIL_WHILE"

        doc "Allocate 1 byte on the stack to read into"
        ppush $ I8 0x00
        doc "Please read"
        x86 $ mov rax $ I64 $ fromIntegral linux_sys_read
        doc "1 char"
        x86 $ mov rdx $ I64 0x01 
        doc "From stdin"
        x86 $ xor rbx rbx
        doc "Into the pstack"
        x86 $ mov rcx rsi
        x86 $ int

        x86 $ cmp rax $ I32 0x00
        x86 $ je "READ_TAIL_W8_ERROR"
       
        doc "Place the char we've just read into rax"
        x86 $ xor rax rax
        ptopW8 al

        doc "Probe the char we just read if it's non-printable "
        doc "then break the loop:"
        x86 $ cmp rax $ asciiI32 ' ' -- Space or less, skip
        x86 $ jleNear "READ_TAIL_W8_BREAK" 
        x86 $ cmp rax $ I32 0x7F -- ESC
        x86 $ jeNear  "READ_TAIL_W8_BREAK"

        x86 $ inc r15
        
        doc "Repeat"
        x86 $ jmpLabel "READ_TAIL_WHILE"

        x86 $ asm $ setLabel "READ_TAIL_W8_BREAK"
        doc "Free the stack from the last allocation:"
        ppopW8 al
        doc "Number of chars read:"
        ppush $ r15
        doc "Success:"
        ppush $ I32 1
        x86 $ ret

        x86 $ asm $ setLabel "READ_TAIL_W8_ERROR"
        doc "Free the stack from the last allocation:"
        ppopW8 al
        doc "Number of chars read:"
        ppush $ r15
        doc "Error:"
        ppush $ I32 0
        x86 $ ret
       

-- Takes 1 parameter and adds input from stdin
-- on the parameters stack as well as the result.
-- http://man7.org/linux/man-pages/man2/read.2.html
-- Type : -> :w64:w64
-- Func : -> :read_char:success
defineRdChrLinux :: Lang ()
defineRdChrLinux = defFunBasic funName body
  where 
    funName = "read_char"
    ty      = TyFunc "READ_CHAR" TyEmpty (TyProd baseWord TyWord)
    body = do
        doc "Allocate 1 byte on the param stack:"
        ppush $ I8 0x00

        x86 $ do 
            docX86 "Read 1 char:"
            mov  rdx $ I64 0x01 
            docX86 "File descriptor 0 <-> read from stdin:"
            xor  rbx rbx
            docX86 "Use the linux_sys_read system call:"
            mov  rax $ I64 $ fromIntegral linux_sys_read
            docX86 "Where to write? To the top of the param stack:"
            mov  rcx rsi
            int

        -- After the call, rax has the number of bytes that were read.
        -- Convert those bytes from the call stack buffer to the parameter
        -- stack, if any.
        x86 $ cmp rax $ I32 0
        x86 $ je "READ_CHARS_NOTHING_READ"

        -- One char read, in rax.
        -- TODO: Assert rax == 1 (This isn't possible for the time being)

        x86 $ xor rax rax
        ppopW8 al -- Take from stack the char we've read

        ppush rax     -- The char we've read
        ppush $ I32 1 -- Success
        returnFunc funName

        x86 $ asm $ setLabel "READ_CHARS_NOTHING_READ"
        ppush $ I32 0 -- Placeholder
        ppush $ I32 0 -- Fail
        returnFunc funName

-- Type :w64       -> :
-- Func :exit_code -> :
-- Exits the program with a specified exit code.
defineExit :: Lang ()
defineExit = defFunBasic "exit" body
    where
        ty   = TyFunc "exit" baseWord TyEmpty
        body = do
            ppop rbx
            x86 $ mov rax $ I64 1
            x86 $ int

-- Type :w8  -> :w8
-- Func :val -> :val
-- Does nothing; as a side effect, it prints the top of the stack on stdout.
defineDbgDumpPtop8 :: Lang ()
defineDbgDumpPtop8 = defFunBasic "dbg_dump_ptop_w8" body
  where
    ty = TyFunc "dbg_dump_ptop_w8" baseWord baseWord
    body = do
        doc "Backup the registers used so as not to affect the caller"
        doc "rax holds the top of the stack."
        ppush rax
        ppeer 1 rax
        ppush rbx
        ppush rcx
        ppush rdx
        doc "rax holds the top of the stack."

        doc "A 8 bit value needs 2 chars to be printed. RCX is the counter."
        x86 $ mov rcx $ I64 2

        x86 $ asm $ setLabel "DBG_DUMP_PTOP_8_START"
        x86 $ mov rbx $ I64 0x10
        x86 $ xor rdx rdx
        x86 $ cmp rcx $ I32 0
        x86 $ je "DBG_DUMP_PTOP_8_END"
        x86 $ div_ rbx
        doc "Print rdx, which holds the remainder."

        x86 $ cmp rdx $ I32 10
        x86 $ jl "DBG_DUMP_PTOP_8_LT10"
        doc "Value between [10 and 15]"
        doc "Add 55 to rdx"
        x86 $ add rdx $ I32 55
        ppush rdx
        x86 $ dec rcx
        x86 $ jmpLabel "DBG_DUMP_PTOP_8_START"

        x86 $ asm $ setLabel "DBG_DUMP_PTOP_8_LT10"
        doc "Value between [0 and 9]"
        doc "Add 0x30 to rdx"
        x86 $ add rdx $ I32 0x30
        ppush rdx
        x86 $ dec rcx
        x86 $ jmpLabel "DBG_DUMP_PTOP_8_START"

        x86 $ asm $ setLabel "DBG_DUMP_PTOP_8_END"
        doc "2 times write char. Conveniently, using the stack before "
        doc "printing reverses the "
        doc "order of the chars (which we need to do)."
        replicateM 2 $ do
            x86 $ callLabel "write_char"
            pdrop 1

        doc "Write a newline char"
        ppush $ I32 0x0A
        x86 $ callLabel "write_char"
        pdrop 1

        doc "Restore all backup registers so as not to affect the caller"
        ppop rdx
        ppop rcx
        ppop rbx
        ppop rax

-- Type :w64 -> :w64
-- Func :val -> :val
-- Does nothing; as a side effect, it prints the top of the stack on stdout.
defineDbgDumpPtop64 :: Lang ()
defineDbgDumpPtop64 = defFunBasic "dbg_dump_ptop_w64" body
  where
    ty = TyFunc "dbg_dump_ptop_w64" baseWord baseWord
    body = do
        doc "Backup the registers used so as not to affect the caller"
        doc "rax holds the top of the stack."
        ppush rax
        ppeer 1 rax
        ppush rbx
        ppush rcx
        ppush rdx

        doc "A 64 bit value needs 16 chars to be printed. RCX is the counter."
        x86 $ mov rcx $ I64 16 

        x86 $ asm $ setLabel "DBG_DUMP_PTOP_64_START"
        x86 $ mov rbx $ I64 0x10
        x86 $ xor rdx rdx
        x86 $ cmp rcx $ I32 0
        x86 $ je "DBG_DUMP_PTOP_64_END"
        x86 $ div_ rbx
        doc "Print rdx, which holds the remainder."

        x86 $ cmp rdx $ I32 10
        x86 $ jl "DBG_DUMP_PTOP_64_LT10"
        doc "Value between [10 and 15]"
        doc "Add 55 to rdx"
        x86 $ add rdx $ I32 55
        ppush rdx
        x86 $ dec rcx
        x86 $ jmpLabel "DBG_DUMP_PTOP_64_START"

        x86 $ asm $ setLabel "DBG_DUMP_PTOP_64_LT10"
        doc "Value between [0 and 9]"
        doc "Add 0x30 to rdx"
        x86 $ add rdx $ I32 0x30
        ppush rdx
        x86 $ dec rcx
        x86 $ jmpLabel "DBG_DUMP_PTOP_64_START"

        x86 $ asm $ setLabel "DBG_DUMP_PTOP_64_END"
        doc "16 times write char. Conveniently, using the stack before "
        doc "printing reverses the "
        doc "order of the chars (which we need to do)."
        replicateM 16 $ do
            x86 $ callLabel "write_char"
            pdrop 1

        doc "Write a newline char"
        ppush $ I32 0x0A
        x86 $ callLabel "write_char"
        pdrop 1

        doc "Restore registers from the backup so as not to affect the caller"
        ppop rdx
        ppop rcx
        ppop rbx
        ppop rax

-- http://man7.org/linux/man-pages/man2/write.2.html
-- Type :w64 -> :int
-- Func :chr -> :success
-- Side effect, prints buf on stdout.
defineWrChrLinux :: Lang ()
defineWrChrLinux = defFunBasic "write_char" body
  where 
    body = do
        x86 $ mov rdx $ I64 1 -- How many bytes to write?
        x86 $ mov rax $ I64 $ fromIntegral linux_sys_write
        x86 $ mov rbx $ I64 $ fromIntegral linux_stdout 
        -- read chars from the top of the params stack, i.e. from our buffer.
        x86 $ mov rcx rsi
        x86 $ int
        pdrop 1   -- Drop the top of the stack which was the char
                  -- we've just printed.
        ppush rax -- The putchar result, how many bytes we've written

defineWrAsciiW8Linux :: Lang ()
defineWrAsciiW8Linux = defFunBasic "write_ascii_w8" body
  where 
    body = do
        x86 $ mov rdx $ I64 1 -- How many bytes to write?
        x86 $ mov rax $ I64 $ fromIntegral linux_sys_write
        x86 $ mov rbx $ I64 $ fromIntegral linux_stdout 
        -- read chars from the top of the params stack, i.e. from our buffer.
        x86 $ mov rcx rsi
        x86 $ int
        pdropW8 1 -- Drop the top of the stack which was the char
                  -- we've just printed.
        ppush rax -- The putchar result, how many bytes we've written

-- Type:      :w64:w64 -> :w64
-- Operation: :a  :b   -> :a<<b
defineShiftLeft = defFunBasic fn body where
    fn   = "SHIFT_LEFT"
    ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
    body = do 
        ppop rcx -- shift
        ppop rax -- number
        x86 $ sal rax cl
        ppush rax

-- Type:      :w64:w64 -> :w64
-- Operation: :a  :b   -> :a<<b
defineShiftRight = defFunBasic fn body where
    fn   = "shift_right"
    ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
    body = do 
        ppop rcx -- shift
        ppop rax -- number
        x86 $ sar rax cl
        ppush rax

definePush1 = defFunBasic fn body where
    fn = "push1"
    ty = TyFunc fn TyEmpty baseWord
    body = do
        ppush $ I32 1

definePushK = defFunBasic fn body where
    fn = "pushk"
    ty = TyFunc fn TyEmpty baseWord
    body = do
        ppush $ I32 75


-- Type:      :w64:w64 -> w64
-- Operation: :a  :b   -> a (x) b
-- defineBinop :: String -> X86_64()
-- FIXME: Think about / introduce IMUL/IPLUS etc for signed/unsigned values!
-- We might need overflow checking also, i.e. return success or failure.
defineBinop fn op
    | fn == "plus" || fn == "minus" || fn == "and" =
    defFunBasic fn body where
        ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
        body = do
            ppop rax
            ppop rbx
            x86 $ op rax rbx
            ppush rax -- Result
defineBinop fn _ 
    | fn == "times" = do
    defFunBasic fn body where
        body = do
            ppop rax
            ppop rbx
            x86 $ mul rbx  -- rax <- rax * rbx
            ppush rax -- Result


defineCMP :: String -> (String -> X86_64()) -> Lang ()
defineCMP funName jmpCmpFun = defFunBasic funName funBody
  where
    funBody = do
        ppop rax
        ppop rbx
        x86 $ cmp rax rbx -- test: rax (?) rbx
        x86 $ jmpCmpFun trueLabel -- if cmpFun holds, jump to trueLabel
        ppush (I32 0)
        x86 $ ret
        x86 $ asm $ setLabel trueLabel
        ppush (I32 1)
        x86 $ ret
    trueLabel = funName ++ "_true"

-- A helper function to jump to the return point of the function.
returnFunc funName = do
    x86 $ jmpLabel $ funName ++ "_return"

runLang :: Lang a 
        -> X86_64 (a, LangState)
runLang stm = runStateT stm initLangState
