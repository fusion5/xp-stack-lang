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

import Data.Int
import Data.Word
import qualified Data.Bits as B
import Control.Monad

-- String hash parameters
fnvOffsetBasis = 0xCBF29CE484222325
fnvPrime       = 0x100000001B3

fnvFold :: Bool -> Word8 -> Word64 -> Word64
fnvFold False x h = (fnvPrime * h) `B.xor` fromIntegral x
fnvFold True  x h = fnvPrime * (h  `B.xor` fromIntegral x)

-- A helper function to compute a fnv-1 hash
fnv1 :: [Word8] -> Word64
fnv1 = foldr (fnvFold False) fnvOffsetBasis

fnv1s :: String -> Word64
fnv1s = fnv1 . map ascii

instance Documentation Lang where
    doc = x86 . doc

callBody :: String -> Lang () 
callBody l = do
    x86 $ callLabel l

defineBaseDefBodies = do
    definePDrop

    defineCMP "EQ"  je
    defineCMP "LT"  jl
    defineCMP "LTE" jle
    defineCMP "GT"  jg
    defineCMP "GTE" jge

    defineBinop "PLUS"  add
    defineBinop "MINUS" sub
    defineBinop "TIMES" undefined
    defineBinop "AND"   and_

    defineShiftLeft
    defineShiftRight

    defineRdChrLinux
    defineWrChrLinux

    defineRdHeadW8
    defineRdTailW8

    defineTermReadLinux

    defineTermHash
    defineTermLook
    defineDUP
    defineNOT
    defineLIT
    defineEMITLIT
    defineEMITCALL
    defineEMITRET
    defineIFPOP0
    defineIFPEEK0
    defineDbgDumpPtop64
    defineDbgDumpPtop8
    defineREPL
    defineREPLDef
    defineREPLDefInt

    definePush1 -- Dummy test function that just pushes constant 1
    definePushK -- Dummy test function that just pushes constant 'K'

    defineExit

    -- defineEval

    -- testSEQDefinitions

data Dict = 
    Entry (Maybe Word64) -- Pointer to the rest of the dictionary, if any.
          Int            -- Function name length
          String         -- Function name
          Word64         -- The address of the body of the function.


pushSimpleDef :: String -> Lang ()
pushSimpleDef defName = do
    doc $ "ASM Dictionary entry for " ++ defName
    doc "ASM or term sequence address in memory:"
    x86 $ mov rax $ L64 defName
    ppush rax
    doc "Definition name hash (for easy search):"
    x86 $ mov rax $ I64 $ fnv1 $ map ascii defName
    ppush rax
    doc "Previous entry pointer:"
    ppush r11
    x86 $ mov r11 rsi

writeMsgHelper msg = do
    doc $ "Write message: " ++ msg
    ppush rax
    ppush rbx
    ppush rcx
    ppush rdx
    x86 $ do
        mov rax $ I64 $ fromIntegral $ linux_sys_write
        mov rbx $ I64 $ fromIntegral $ linux_stderr
        mov rdx (I64 $ fromIntegral $ length msg) 
        asm $ addString msg
        mov rcx (S64 msg)
        int
    ppop rdx
    ppop rcx
    ppop rbx
    ppop rax

pushBaseDict = do
    doc "Simple Dictionary of cells [ prev ][ name ][ addr ]"
    x86 $ mov r11 $ I64 0
    pushSimpleDef "NOT"
    pushSimpleDef "PDROP"
    pushSimpleDef "EQ"
    pushSimpleDef "LT"
    pushSimpleDef "LTE"
    pushSimpleDef "GT"
    pushSimpleDef "GTE"
    pushSimpleDef "MINUS"
    pushSimpleDef "TIMES"
    pushSimpleDef "AND"
    -- pushSimpleDef "LIT"
    pushSimpleDef "EXIT"
    -- pushSimpleDef "DUP"
    pushSimpleDef "PLUS"
    pushSimpleDef "PUSH1"
    pushSimpleDef "PUSHK"
    pushSimpleDef "WRITE_CHAR"
    pushSimpleDef "READ_CHAR"
    pushSimpleDef "READ_PRINTABLES_W8"
    pushSimpleDef "DBG_DUMP_PTOP_64"
    pushSimpleDef "TERM_LOOK"
    pushSimpleDef "TERM_HASH"
    pushSimpleDef "REPL"
    doc "Dictionary end marking point (LOOKUP ends here)"
    ppushI32 0

-- Bootstrap a kind of Forth interpreter dictionary.
-- The dictionary is a linked list of definitions. A definition consists
-- of a (k, prev) := body address
-- When interpreting an expression, we use the dictionary entries to find
-- the definitions of terms in the expression.
{-
pushInitialDictionary = do
    doc "Initialize r11, the dictionary pointer, as the empty dictionary:"
    x86 $ mov r11 $ I64 0
    pushASMDef "NOT"
    pushASMDef "PDROP"
    pushASMDef "EQ"
    pushASMDef "LT"
    pushASMDef "LTE"
    pushASMDef "GT"
    pushASMDef "GTE"
    pushASMDef "MINUS"
    pushASMDef "TIMES"
    pushASMDef "AND"
    pushASMDef "LIT"
    pushASMDef "EXIT"
    pushASMDef "DUP"
    pushASMDef "PLUS"
    pushASMDef "PUSH1"
    pushASMDef "WRITE_CHAR"
    pushASMDef "READ_CHAR"
    pushASMDef "IFPOP0" -- Pops and executes the next word only if the popped
                        -- value equals 0.
    pushASMDef "IFPEEK0" -- Peeks and executes the next word only if the top
                         -- stack value equals 0.
    pushASMDef "DBG_DUMP_PTOP_64"
    pushSEQDef "PUSH3" [] [lit 3]
    pushSEQDef "PUSH2" [] [run "PUSH1", run "PUSH1", run "PLUS"]
    pushSEQDef "BETWEEN" 
        [("num", baseWord), ("low", baseWord), ("hi", baseWord)] 
        [
            -- TODO: Think of a way of referring to those parameters
            -- from the call stack, possibly using a textual reference...
            -- (2020-02-26 19:35)
            -- par "num",
            -- par "low",
            lit 5
        ]
    pushSEQDef "SHOW_BYTE_0_9" [] [
        -- we have the value on the stack.
        lit 0x30, -- '0'
        run "PLUS",
        run "WRITE_CHAR"
        ]
    pushSEQDef "SHOW_BYTE_10_15" [] [
        lit 0x37, -- 'A' - 10
        run "PLUS",
        run "WRITE_CHAR"
        ]
    pushSEQDef "SHOW_HEX" [] [
        -- On the stack we have a value between 0 and 15.
        -- Convert it to an ASCII value of its hex representation.
        -- 0-9 bound check.
        run "DUP",
        lit 0,
        lit 9,
        run "BETWEEN", 
        run "NOT",
        run "IFPOP0",
        run "SHOW_BYTE_0_9",
        run "DUP",
        lit 10,
        lit 15,
        run "BETWEEN",
        run "NOT",
        run "IFPOP0",
        run "SHOW_BYTE_10_15" 
        ]
    pushSEQDef "MAIN" [] [
            -- run "PUSH3",   run "PUSH3", run "TIMES", 
            -- lit 9,         run "EQ",    lit 1, run "MINUS",
            -- run "IFPEEK0", run "PUSH2", run "EXIT"
            lit 0,
            run "BETWEEN",
            run "DBG_DUMP_PTOP_64",
            run "EXIT"
        ]
-}

{-
appendSEQDefinition :: Maybe String -> String -> X86_64 String
appendSEQDefinition prevLabel bodyLabel = do
    docX86 $ "Dictionary entry for " ++ bodyLabel
    let x = "DICT_" ++ bodyLabel
    asm $ setLabel x
    case prevLabel of
        Just l  -> asm $ emitLabelRef64 l
        Nothing -> imm64 0
    asm $ bflush
    docX86 $ "Type:"
    imm64 1 -- Sequence-based definition (to be evaluated using EVAL)
    asm $ bflush
    docX86 $ "Seq address (will the seq reside on the stack also sometimes?):"
    asm $ emitLabelRef64 $ bodyLabel
    asm $ bflush
    -- TODO: throw error if bodyLabel exceeds 255 chars!
    docX86 $ "Label length:"
    imm64 $ fromIntegral $ length bodyLabel
    asm $ bflush
    docX86 $ "Dictionary entry label:"
    asm $ emitString bodyLabel
    asm $ bflush
    return x
-}

-- This is a sort of struct of C...
appendASMDefinition :: Maybe String -> String -> X86_64 String
appendASMDefinition prevLabel bodyLabel = do
    docX86 $ "Dictionary entry for " ++ bodyLabel
    x <- asm $ freshLabelWithPrefix $ "DICT_ENTRY_" ++ bodyLabel ++ "_"
    docX86 $ "Previous entry ptr:"
    case prevLabel of
        Just l  -> asm $ emitLabelRef64 l
        Nothing -> imm64 0
    asm $ bflush
    docX86 $ "Hash:"
    imm64 $ fnv1 $ map ascii bodyLabel
    asm $ bflush
    -- TODO: Add the hash to assist in the term search!
    docX86 $ "Type:"
    imm64 0 -- 0 means it's an ASM-based definition
    asm $ bflush
    docX86 $ "ASM code address:"
    asm $ emitLabelRef64 bodyLabel
    asm $ bflush
    -- TODO: throw error if bodyLabel exceeds 255 chars!

    docX86 $ "Dictionary entry label:"
    asm $ emitString bodyLabel
    asm $ bflush
    docX86 $ "Label length:"
    imm64 $ fromIntegral $ length bodyLabel
    asm $ bflush
    return x

pushASMDef :: String -> Lang ()
pushASMDef bodyLabel = do
    doc $ "ASM Dictionary entry for " ++ bodyLabel
    -- x <- freshLabelWithPrefix $ "DICT_ENTRY_" ++ bodyLabel ++ "_"
    -- x86 $ asm $ setLabel x
    -- doc "Label:"
    -- mapM ppush $ map (I8 . ascii) bodyLabel
    -- doc "Label length:"
    -- ppush $ I32 $ fromIntegral $ length bodyLabel
    doc "ASM or term sequence address in memory:"
    x86 $ mov rax $ L64 bodyLabel
    ppush rax
    doc "Type (0 means it's an ASM-based definition):"
    ppush $ I32 0
    doc "Unused (for alignment purposes with SEQ definitions)"
    ppush $ I32 0
    doc "Hash (for easy search):"
    x86 $ mov rax $ I64 $ fnv1 $ map ascii bodyLabel
    ppush rax
    doc "Previous entry pointer:"
    ppush r11
    doc "Set the dictionary pointer as the top of the stack, since"
    doc "the stack now holds a dictionary definition which will"
    doc "remain there."
    doc "It should be an invariant that the parameter stack top,"
    doc "rsi is always >= r11."
    x86 $ mov r11 rsi

pushSeqItem :: [SeqParam] -> SeqInstr -> Lang ()
pushSeqItem _  (SeqDefTerm term) = do
    doc "Push the term to look up"
    mapM ppush $ map (I8 . ascii) term
    ppush $ I32 $ fromIntegral $ length term
    doc "Look it up"
    x86 $ callLabel "TERM_LOOK"
    -- Commented for debug purposes (more compact code)
    -- assertPtop 1 "Undefined dictionary reference"
    pdrop 1
pushSeqItem _ (SeqDefLit64 n) = do
    -- As usual, things on the stack go on reverse.
    -- First we add the literal value that we wish to push on
    -- the stack.
    doc "Build a LIT function call. The literal comes first"
    doc "on the stack in the call sequence."
    x86 $ mov rax $ I64 n
    ppush $ rax
    doc "Then follows the LIT call."
    pushSeqItem [] (SeqDefTerm "LIT")
pushSeqItem params (SeqDefParam term) = do
    doc "A parameter reference: a function needs to peer down "
    doc "the parameter stack and push onto pstack. Here we " 
    doc "compute the offset and the length that needs to be pushed "
    doc "on the stack."
    case lookup term paramOffsets of
        Nothing     -> x86 $ asm $ throwError $ 
            "Undefined parameter reference: " ++ term
        Just offset -> x86 $ asm $ throwError "Unimplemented"
        -- TODO: Define a CPEER (:w64:w64) that pushes on the stack 
        -- a parameter from the call stack
        -- (2020-02-28)
    where paramOffsets = computeOffsets params

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

-- In the end this function will have to be implemented in the language
-- itself...
pushSEQDef :: String 
           -> [SeqParam] 
           -> [SeqInstr] 
           -> Lang ()
pushSEQDef bodyLabel params calls = do
    -- TODO: Resolve the dictionary pointers of the parameters
    -- in 'calls' and emit them on the stack as well. 
    -- Then have the def.addr point to -- this stack sequence.
    doc $ "SEQ body definition for " ++ bodyLabel
    doc "With stacks we have to do everything backwards."
    doc "Push a 0 to indicate the termination of the sequence! (Essential)"
    doc "Then push the call sequence in reverse order."
    ppush $ I32 0
    -- Reverse bc. the stack grows opposite to addresses
    mapM (pushSeqItem params) $ reverse calls
    doc $ "SEQ dictionary entry for " ++ bodyLabel
    -- x <- freshLabelWithPrefix $ "DICT_ENTRY_" ++ bodyLabel ++ "_"
    -- let x = "DICT_" ++ bodyLabel
    -- x86 $ asm $ setLabel x
    -- doc $ "Label:"
    -- mapM ppush $ map (I8 . ascii) bodyLabel
    -- doc $ "Label length:"
    -- ppush $ I32 $ fromIntegral $ length bodyLabel

    -- TODO: Since this is always the previous address on the stack, we 
    -- could just drop this parameter from SEQ definitions and have the
    -- sequence words follow immediately...
    --
    doc "Cache rsi into rbx to avoid ppush function from interfering"
    x86 $ mov rbx rsi
    doc $ "Term sequence memory address (it's the stack top because we "
    doc $ "just pushed it on the stack):"
    ppush rbx
    doc "Add the length of the parameters on the stack (just it for now)"
    ppush $ I32 $ sum $ map (sizeof . snd) params
    doc $ "Type (1 means it's an SEQ-based definition):"
    ppush $ I32 1
    doc $ "Hash (for easier search):"
    x86 $ mov rax $ I64 $ fnv1 $ map ascii bodyLabel
    ppush rax
    doc $ "Previous entry pointer:"
    ppush r11
    doc $ "Advance the dictionary:"
    x86 $ mov r11 rsi

defineDUP :: Lang () 
defineDUP = defFunBasic funName ty body
  where
    funName = "DUP"
    ty      = undefined
    body    = do
        doc "Duplicate the top item on the stack."
        ppeek rax
        ppush rax
        x86 $ ret

defineNOT :: Lang ()
defineNOT = defFunBasic funName ty body
  where
    funName = "NOT"
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


-- Type : -> :w64:w64
-- Func : -> :term_address:success_or_failure
defineTermLook :: Lang ()
defineTermLook = defFunBasic funName ty body
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
        ppop rax 
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

defineIFPEEK0 :: Lang ()
defineIFPEEK0 = defFunBasic funName ty body
  where
    funName = "IFPEEK0"
    ty = undefined
    body = do
        -- Like if POP0 but it doesn't pop.
        doc "Only execute the next word (from register r8) if the top"
        doc "of the stack equals 0."
        ppeek rax
        x86 $ cmp  rax (I32 0)
        x86 $ je "IFPEEK0_EQUALS"
        doc "Skip the next word by adding 8 bytes to r8."
        doc "FIXME: What if the next word is out of the sequence"
        doc "under evaluation?"
        x86 $ add r8 (I32 8)
        x86 $ ret
        x86 $ asm $ setLabel "IFPEEK0_EQUALS"
        x86 $ ret

defineIFPOP0 :: Lang () 
defineIFPOP0 = defFunBasic funName ty body
  where
    funName = "IFPOP0"
    ty = undefined
    body = do
        doc "Only execute the next word (from register r8) if the top"
        doc "of the stack equals 0."
        ppop rax
        x86 $ cmp  rax (I32 0)
        x86 $ je "IFPOP0_EQUALS"
        doc "Skip the next word by adding 8 bytes to r8."
        doc "FIXME: What if the next word is out of the sequence"
        doc "under evaluation?"
        x86 $ add r8 (I32 8)
        x86 $ ret
        x86 $ asm $ setLabel "IFPOP0_EQUALS"
        x86 $ ret

defineLIT :: Lang ()
defineLIT = defFunBasic funName ty body
  where
    funName = "LIT"
    ty = undefined
    body = do
        doc "Push a literal value on the stack present on the next"
        doc "word (from register r8)."
        x86 $ add r8  (I32 8)
        x86 $ mov rax (derefOffset r8 0)
        ppush rax
        x86 $ ret

-- Type: w8 :w8 :..:w8   :w64 -> w64                                       :w64
-- Func: sgn:w_0:..:w_n-1:n   -> sgn*(w_n-1 + w_n-2*10 + ... w_0*10^n-1):success
-- Convert an integer string to an integer.
defineREPLDefInt :: Lang () 
defineREPLDefInt = defFunBasic "REPL_DEF_INT" undefined body
  where
    body = do
        doc "Parse an integer that's part of the REPL definition."
        
        writeMsgHelper "String length:\n"

        ppop rax -- The length of the string.
        
        x86 $ asm $ setLabel "REPL_DEF_INT_LOOP"

        x86 $ cmp rax $ I32 0

        x86 $ jeNear "REPL_DEF_INT_END"
        writeMsgHelper "REPL_DEF_INT STEP\n"
        x86 $ callLabel "DBG_DUMP_PTOP_8"

        x86 $ xor rdx rdx
        ppopW8 dl -- Drop the 8-bit word from the pstack.
        x86 $ dec rax

        x86 $ jmpLabel "REPL_DEF_INT_LOOP"

        x86 $ asm $ setLabel "REPL_DEF_INT_END"

        ppush $ I32 1
        x86 $ ret

defineREPLDef :: Lang ()
defineREPLDef = defFunBasic "REPL_DEF" undefined body
  where
    body = do
        doc "REPL Definition subfunction."
        doc "This is the REPL part that reads new definitions."
        doc "It is not callable directly from the dictionary, it's "
        doc "only called from REPL."

        doc "Read and hash the term being defined:"
        x86 $ callLabel "READ_PRINTABLES_W8"
        assertPtop 1 "Could not read the term to define!\n"
        pdrop 1
        x86 $ callLabel "TERM_HASH"

        doc "Read a char, and ensure it is the equal sign:"
        x86 $ callLabel "READ_HEAD_W8"
        assertPtop 1 "Could not read a printable char!\n"
        pdrop 1

        x86 $ xor rax rax
        ppopW8 al -- The char returned by READ_HEAD_W8
        doc "The ascii code for '=' is 0x3D"
        x86 $ mov rbx $ I64 $ 0x3D
        x86 $ cmp rax rbx
        x86 $ jeNear "REPL_DEF_READ_TERMS"
        doc "We have not read the equal sign. Return"
        writeMsgHelper "Error: expecting an '=' character after the term.\n"
        x86 $ ret
        x86 $ asm $ setLabel "REPL_DEF_READ_TERMS"

        ppop rax -- The hash of the term we're defining
        doc "Build our [ prev ][ name hash ][ addr ] record on the stack"
        doc "for the new dictionary entry..."
        doc "Unfortunately this assumes that the operation will"
        doc "succeed. FIXME: roll back "
        doc "the entire operation if the definition is in some way"
        doc "broken (e.g. missing term)"
        ppush r9  -- addr
        ppush rax -- name hash 
        ppush r11 -- prev
        x86 $ mov r11 rsi

        x86 $ asm $ setLabel "REPL_DEF_READ_TERMS_LOOP"

        writeMsgHelper "REPL_DEF_READ_TERMS_LOOP LOOP \n"

        {-
        x86 $ callLabel "READ_PRINTABLES_W8"
        assertPtop 1 "Failed to parse term within definition body\n"
        pdrop 1
        -}

        x86 $ callLabel "READ_HEAD_W8"
        assertPtop 1 "READ_HEAD_W8 failed\n"
        pdrop 1

        doc "Patern match on the stack top (READ HEAD W8)."

        x86 $ xor rdx rdx
        ppeerW8 0 dl -- Top of the stack W8

        ppush rdx 
        writeMsgHelper  "First character HEX code:\n"
        x86 $ callLabel "DBG_DUMP_PTOP_64"
        ppop rdx
        
        doc "Is the RDX char greater than ascii code 9?"
        x86 $ cmp rdx $ I32 0x39
        x86 $ jgNear "REPL_DEF_NOT_NUMERIC"

        doc "Is the RDX char less than ascii code 0?"
        x86 $ cmp rdx $ I32 0x30 
        x86 $ jlNear "REPL_DEF_NOT_NUMERIC"

        doc "Our term starts with 0-9. Therefore, "
        doc "attempt to parse a Positive Integer Literal."

        -- Read the rest of the input, until whitespace ...
        x86 $ callLabel "READ_TAIL_W8"
        assertPtop 1 "READ_TAIL_W8 was not successful!"
        pdrop 1

        ppop rbx
        x86 $ inc rbx
        ppush rbx

        x86 $ callLabel "REPL_DEF_INT"
        assertPtop 1 "REPL_DEF_INT was not successful!"

        x86 $ jmpLabel "REPL_DEF_READ_TERMS_LOOP"

        -- rdx now contains the first character of the string.
        x86 $ asm $ setLabel "REPL_DEF_NOT_NUMERIC"

        -- Place the rest of the input to the stack...
        x86 $ callLabel "READ_TAIL_W8"
        assertPtop 1 "READ_TAIL_W8 was not successful!"
        pdrop 1

        ppop rbx
        x86 $ inc rbx
        ppush rbx

        -- TODO: Refactor: move this into REPL_DEF_REFERENCE (A reference to 
        -- another term in the dictionary).
        x86 $ callLabel "TERM_LOOK"
        ppop rax
        -- If we have read an unknown term, end the loop.
        -- TODO: If we have read a '.' then end the loop (mark the
        -- end of the definition somehow).
        doc "If we have read an unknown term, then check whether we"
        doc "can parse it as an integer."
        x86 $ cmp rax (I32 0)
        x86 $ je "REPL_DEF_READ_TERMS_LOOP_END"

        doc "Successful term read, emit a call command to the"
        doc "term address:"
        x86 $ callLabel "EMIT_CALL"
        x86 $ jmpLabel  "REPL_DEF_READ_TERMS_LOOP"

        doc "Where we break from the read loop:"
        x86 $ asm $ setLabel "REPL_DEF_READ_TERMS_LOOP_END"

        x86 $ callLabel "EMIT_RET"
        writeMsgHelper "Definition added.\n"
        x86 $ ret

defineREPL :: Lang ()
defineREPL = defFunBasic "REPL" undefined body
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
        -- x86 $ callLabel "DBG_DUMP_PTOP_64"

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
        writeMsgHelper "Undefined term!\n"
        x86 $ jmpLabel "REPL_START"
         

        x86 $ asm $ setLabel "REPL_QUIT"
        
        
-- (w64 dict_address:w64 n -- :)
defineEMITCALL :: Lang () 
defineEMITCALL = defFunBasic "EMIT_CALL" undefined body
  where
    body = do
        doc "Emit a call to a certain dictionary entry, the address of which"
        doc "is on the stack."

        ppop rax
        x86 $ mov rax (derefOffset rax 16)

        -- CALL using E8 WARNING! THis uses relative addresses!
        -- It's trickier (but we will have to optimize this probably)
        -- https://stackoverflow.com/questions/19552158
        -- rax = 00000000C00008EB
        {-
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
defineEMITRET = defFunBasic "EMIT_RET" undefined body 
  where
    body = do
        doc "Emit a return statement which should be always present at "
        doc "the end of an assembly body definition."
        doc "X86: ret"
        x86 $ do
            mov (derefOffset r9 0) (I8 0xC3)
            inc r9
           
defineEMITLIT :: Lang ()
defineEMITLIT = defFunBasic funName ty body
  where
    funName = "EMIT_LIT_64"
    ty      = undefined
    body    = do
        doc "Emit assembly code in the dynamic code area"
        doc "That pushes a 64bit literal value on the stack."
        x86 $ xor rax rax
        ppop rax -- Take the literal that we need to push and place it in rax

        -- TODO: Improve language (repeated code)
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
definePTopWrite = defFunBasic funName ty body
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
        

defineTermHash :: Lang ()
defineTermHash = defFunBasic funName ty body
  where
    -- Improve: Is TESH_HARM expressible as a sequence of more basic words?
    funName = "TERM_HASH"
    ty      = undefined -- Unexpressible atm.
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
defineConsumeWhitespace = defFunBasic funName ty body
  where
    funName = ""
    ty      = undefined
    body    = do
        doc "Consume input characters "

-- Type: -> w8 :...:w8   :w64:w64
-- Func: -> w_0:...:w_n-1:n  :success
-- Pushes a sequence of printable characters that come from stdin
defineTermReadLinux :: Lang ()
defineTermReadLinux = defFunBasic funName ty body
  where
    funName = "READ_PRINTABLES_W8"
    ty      = undefined -- Unexpressible atm.
    body    = do
        doc "READ_HEAD_W8 consumes any whitespace present before"
        doc "the term and it pushes the first encountered non-ws char."
        x86 $ callLabel "READ_HEAD_W8"

        -- x86 $ writeMsgHelper "READ_HEAD_W8 called, it returned:\n"
        -- x86 $ callLabel "DBG_DUMP_PTOP_64"
        assertPtop 1 "READ_HEAD_W8 failed\n"
        pdrop 1

        -- x86 $ writeMsgHelper "This is what it read:\n"
        -- x86 $ callLabel "DBG_DUMP_PTOP_64"

        doc "Reads characters until a space or a control "
        doc "char (non-printable) is read."
        x86 $ callLabel "READ_TAIL_W8"
        ppop rax
        ppop rbx

        doc "Increment the number of characters reutnred by "
        doc "READ_TAIL_W8 to account for the extra char on "
        doc "the pstack placed by READ_HEAD_W8 above."
        x86 $ inc rbx
        ppush rbx
        ppush rax

-- Parses input. Skips whitespace, control characters, etc. 
-- Stops at the first non-such char and pushes it on the stack.
-- Type : -> :w8        :w64
-- Func : -> :read_char:success
defineRdHeadW8 :: Lang ()
defineRdHeadW8 = defFunBasic funName undefined body
  where
    funName = "READ_HEAD_W8"
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

defineRdTailW8 :: Lang ()
defineRdTailW8 = defFunBasic funName ty body
  where
    funName = "READ_TAIL_W8"
    ty = undefined
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
        x86 $ cmp rax $ I32 0x20 -- Space or less, skip
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
defineRdChrLinux = defFunBasic funName ty body
  where 
    funName = "READ_CHAR"
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
defineExit = defFunBasic "EXIT" ty body
    where
        ty   = TyFunc "EXIT" baseWord TyEmpty
        body = do
            ppop rbx
            x86 $ mov rax $ I64 1
            x86 $ int

-- Type :w8  -> :w8
-- Func :val -> :val
-- Does nothing; as a side effect, it prints the top of the stack on stdout.
defineDbgDumpPtop8 :: Lang ()
defineDbgDumpPtop8 = defFunBasic "DBG_DUMP_PTOP_8" ty body
  where
    ty = TyFunc "DBG_DUMP_PTOP_8" baseWord baseWord
    body = do
        doc "rax holds the top of the stack."
        ppeek rax
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
            x86 $ callLabel "WRITE_CHAR"
            pdrop 1

        doc "Write a newline char"
        ppush $ I32 0x0A
        x86 $ callLabel "WRITE_CHAR"
        pdrop 1


-- Type :w64 -> :w64
-- Func :val -> :val
-- Does nothing; as a side effect, it prints the top of the stack on stdout.
defineDbgDumpPtop64 :: Lang ()
defineDbgDumpPtop64 = defFunBasic "DBG_DUMP_PTOP_64" ty body
  where
    ty = TyFunc "DBG_DUMP_PTOP_64" baseWord baseWord
    body = do
        doc "rax holds the top of the stack."
        ppeek rax
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
            x86 $ callLabel "WRITE_CHAR"
            pdrop 1

        doc "Write a newline char"
        ppush $ I32 0x0A
        x86 $ callLabel "WRITE_CHAR"
        pdrop 1


-- http://man7.org/linux/man-pages/man2/write.2.html
-- Type :w64 -> :int
-- Func :chr -> :success
-- Side effect, prints buf on stdout.
defineWrChrLinux :: Lang ()
defineWrChrLinux = defFunBasic "WRITE_CHAR" ty body
  where 
    ty   = TyFunc "WRITE_CHAR" baseWord baseWord
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

-- Type:      :w64:w64 -> :w64
-- Operation: :a  :b   -> :a<<b
defineShiftLeft = defFunBasic fn ty body where
    fn   = "SHIFT_LEFT"
    ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
    body = do 
        ppop rcx -- shift
        ppop rax -- number
        x86 $ sal rax cl
        ppush rax

-- Type:      :w64:w64 -> :w64
-- Operation: :a  :b   -> :a<<b
defineShiftRight = defFunBasic fn ty body where
    fn   = "SHIFT_RIGHT"
    ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
    body = do 
        ppop rcx -- shift
        ppop rax -- number
        x86 $ sar rax cl
        ppush rax

definePush1 = defFunBasic fn ty body where
    fn = "PUSH1"
    ty = TyFunc fn TyEmpty baseWord
    body = do
        ppush $ I32 1

definePushK = defFunBasic fn ty body where
    fn = "PUSHK"
    ty = TyFunc fn TyEmpty baseWord
    body = do
        ppush $ I32 75


-- Type:      : -> :w64
-- Operation: : -> :literal
{-
defineLit = defFunBasic fn ty body where
    fn = "LIT"
    ty = TyFunc fn TyEmpty baseWord
    body = do
        doc "Look up the call stack to find out the position from which"
        doc "LIT was called:"
        x86 $ ctop rax
        doc "Place on rbx the word found after the parent LIT reference:"
        x86 $ mov rbx (derefOffset rax 0)
        doc "Push the value from rbx (the literal) onto the stack."
        ppush rbx
        doc "Manipulate the call stack so that the RET that follows"
        doc "skips the literal we have just read (which by no means"
        doc "should be returned to and executed!)"
        x86 $ add rsp $ I32 8
-}

{- Commented because these are pushed to the stack as well in ASM def
-- A sequence definition attempt that we want to evaluate.
-- TODO: Push these to the stack as well.
testSEQDefinitions :: Lang ()
testSEQDefinitions = do
    x86 $ asm $ setLabel "TEST_SEQUENCE1"
    -- Emit a dictionary reference to PUSH3... this should
    -- be obtained through a dictionary search, since we don't
    -- have the exact dictionary addresses of the words.
    x86 $ mov rax $ I64 $ fnv1 $ map ascii "PUSH3"
    ppush rax

    x86 $ do 
        asm $ emitLabelRef64 "DICT_PUSH3"
        asm $ bflush
        asm $ emitLabelRef64 "DICT_PLUS"
        asm $ bflush
        asm $ emitLabelRef64 "DICT_PLUS"
        asm $ bflush
        asm $ emitLabelRef64 "DICT_EXIT"
        asm $ bflush
        imm64 0 -- Marks the sequence end
        asm $ bflush

    x86 $ do
        asm $ setLabel "PUSH3"
        asm $ emitLabelRef64 "DICT_PUSH1"
        asm $ bflush
        asm $ emitLabelRef64 "DICT_PUSH1"
        asm $ bflush
        asm $ emitLabelRef64 "DICT_PUSH1"
        asm $ bflush
        imm64 0 -- Marks the sequence end
        asm $ bflush
-}

{-
defineEval :: Lang ()
defineEval = do
    x86 $ asm $ setLabel "EVAL"
    -- TODO: Define some sort of struct type/GADT and use that to access
    -- the dictionary entry fields maybe, to handle more complexity?
    -- RAX is a pointer to the sequence to be evaluated.
    -- The first entry in the sequence is a dictionary entry.
    -- If the value at address RAX is 0, then we have an ASM-based entry. 
    -- FIXME: Eval has a bug, it claims to use r9 to iterate through the
    -- dictionary entry r8, but it doesn't increment r9 once done, it 
    -- increments r8! I don't think r9 is needed!
    doc "r8 holds the term (dict. pointer) under evaluation. It's a"
    doc "pointer to an array of terms that ends in 0."
    doc "r9 is the dictionary entry pointed by r8:"
    x86 $ mov r9 (derefOffset r8 0)

    x86 $ cmp r9 (I32 0x00)  -- If r8 points to 0...
    x86 $ jeNear "EVAL_STOP" -- Empty word reached - reached the sequence end.

    doc "r9 + 0:  .prev (-ious) dictionary entry, if any"
    doc "r9 + 8:  .hash of rntry name, fnv1 for easy search"
    doc "r9 + 16: .type of dictionary entry (0=asm, 1=words or typedef?)"
    doc "r9 + 24: .len  of parameters on the stack"
    doc "r9 + 32: .addr of ASM or term sequence in memory"

    doc "Move def.type to r10"
    x86 $ mov r10 (derefOffset r9 16)

    doc "Pattern match def.type:"
    -- TODO: Could we avoid the r10 register?
    -- can we do: cmp (derefOffset r9 16) (I32 0x00)
    x86 $ cmp r10 (I32 0x00)
    x86 $ jeNear "EVAL_ASM"
    x86 $ cmp r10 (I32 0x01)
    x86 $ jeNear "EVAL_SEQ"

    doc "def.type == 0: Evaluating assembly"
    doc "Move def.addr to r10. Assume the ASM doesn't modify r8."
    x86 $ do
        asm $ setLabel "EVAL_ASM"
        mov  r10 (derefOffset r9 32)
        call r10 
        jmpLabel "EVAL_STEP_DONE"

    doc "def.type == 1: Evaluating a sequence of words"
    doc "Recursively call EVAL on each sequence word."
    doc "The sequence to be called is at r9 + 32 (.addr)."
    doc "Prepare for eval recursion: save the current r8 on the "
    doc "call stack and make r8 now point to the first term in "
    doc "the sequence. "
    x86 $ do 
        asm $ setLabel "EVAL_SEQ"

        docX86 "Copy the parameters of length r9 + 24 (.len) from the pstack "
        docX86 "onto the cstack. The purpose is to allow us to refer to any "
        docX86 "received parameter by the sequence without worrying about "
        docX86 "stack alteration. This removes the necessity of dup, etc."

        docX86 "rcx is our loop counter. Initialize to .len"
        mov rcx (derefOffset r9 24) 

        -- Memcpy from pstack onto cstack:
 
        asm $ setLabel "EVAL_SEQ_LOOP_START"
        docX86 "Has our loop counter finished?"
        cmp rcx $ I32 0 -- rcx is our loop counter
        je "EVAL_SEQ_LOOP_END"

        docX86 "Peer down the pstack using rax: "
        docX86 "First compute the pstack address into rax."
        mov rax rsi
        add rax rcx
        docX86 "Place the byte at rax into bl"
        mov bl $ derefOffset rax 0

        docX86 "Push bl onto cstack"
        sub rsp $ I32 1
        mov (derefOffset rsp 0) bl

        dec rcx -- Subtract one byte

        jmpLabel "EVAL_SEQ_LOOP_START"
        asm $ setLabel "EVAL_SEQ_LOOP_END"

        push r8
        push r9
        mov  r8 (derefOffset r9 32)
        -- TODO: I think that on the call stack we need to
        -- push the function parameters that we're evaluating too, so that
        -- it can refer to them without needing dup/swap,etc...
        -- movParamsToCallStackW64s from the old version;
        -- But this depends on the type and we don't know the definition 
        -- data-type for now. This should be done AFTER we add types.
        callLabel "EVAL" -- Recursion
        pop r9
        pop r8

        docX86 "Pop from the cstack the parameters we have pushed prior to"
        docX86 "the call (.len):"
        add rsp (derefOffset r9 24)
        
        jmpLabel "EVAL_STEP_DONE"

    x86 $ asm $ setLabel "EVAL_STEP_DONE"

    -- Move on to the next word in the sequence we're evaluating.
    doc "Advance the word pointer to the next word and loop back to EVAL."
    x86 $ add r8 (I32 8)
    x86 $ jmpLabel "EVAL"

    x86 $ asm $ setLabel "EVAL_STOP"
    x86 $ ret 
-}

-- Type:      :w64:w64 -> w64
-- Operation: :a  :b   -> a (x) b
-- defineBinop :: String -> X86_64()
-- FIXME: Think about / introduce IMUL/IPLUS etc for signed/unsigned values!
-- We might need overflow checking also, i.e. return success or failure.
defineBinop fn op
    | fn == "PLUS" || fn == "MINUS" || fn == "AND" =
    defFunBasic fn ty body where
        ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
        body = do
            ppop rax
            ppop rbx
            x86 $ op rax rbx
            ppush rax -- Result
defineBinop fn _ 
    | fn == "TIMES" = do
    defFunBasic fn ty body where
        ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
        body = do
            ppop rax
            ppop rbx
            x86 $ mul rbx  -- rax <- rax * rbx
            ppush rax -- Result


defineCMP :: String -> (String -> X86_64()) -> Lang ()
defineCMP funName jmpCmpFun = defFunBasic funName ty funBody
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
    ty = TyFunc funName (TyProd baseWord TyWord) baseWord
    trueLabel = funName ++ "_true"

definePDrop :: Lang ()
definePDrop = defFunBasic "PDROP" ty body
    where body = pdrop 1
          ty   = TyFunc "PDROP" baseWord TyEmpty

-- Parameter stack drop, alters the parameter stack register rsi
pdrop :: Int32 -> Lang ()
pdrop numW64s =
    x86 $ add rsi $ I32 $ fromIntegral $ numW64s * 8

pdropW8 :: Int32 -> Lang ()
pdropW8 numW8s = 
    x86 $ add rsi $ I32 $ fromIntegral $ numW8s

-- Parameter stack pop
ppop :: Val -> Lang ()
ppop dst64@(R64 _) = do
    doc $ "ppop " ++ show dst64
    x86 $ mov dst64 $ derefOffset rsi 0
    x86 $ add rsi $ I32 8

ppeek = ppeer 0

ppeer :: Int32 -> Val -> Lang ()
ppeer numW64s dst =
    x86 $ mov dst $ derefOffset rsi (fromIntegral $ numW64s * 8)

ppeerW8 :: Int32 -> Val -> Lang ()
ppeerW8 numW8s dst@(R8 r)  =
    x86 $ mov dst $ derefOffset rsi (fromIntegral numW8s)
ppeerW8 _ _ = error "ppeerW8 requires an 8-bit register as parameter"

ptopW8 :: Val -> Lang ()
ptopW8 = ppeerW8 0

ppopW8 :: Val -> Lang ()
ppopW8 dst@(R8 reg) = x86 $ do
    mov dst $ derefOffset rsi 0
    add rsi $ I32 1

cpeerW8 :: Int32 -> Val -> X86_64 ()
cpeerW8 numW8s dst =
    mov dst $ derefOffset rsp (fromIntegral numW8s)

cpeer :: Int32 -> Val -> X86_64 ()
cpeer numW64s dst = 
    mov dst $ derefOffset rsp (fromIntegral $ numW64s * 8)

ctop :: Val -> X86_64 ()
ctop = cpeer 0

-- Parameter push on the parameter stack (pstack), a different 
-- stack from the call stack (for which we use simply push).
-- For the parameter stack position, we reserve the RSI register.
-- Programs should avoid this register!
-- void -> 8 bytes space of any type
-- TODO: Consider the benefits of only supporting w64 on pstack and have 
-- another stack for w8?
-- FIXME: Rather than RSI use RBP which isn't used anyway.
ppush :: Val -> Lang ()
ppush v | supported v = do
    doc $ "ppush " ++ show v
    x86 $ sub rsi $ I32 $ sz v
    x86 $ mov (derefOffset rsi 0) v
    where sz (I64 _)    = 8
          sz (R64 _)    = 8
          sz (RR64 _ _) = 8
          sz (L64 _)    = 8
          sz (I32 _)    = 8
          sz (I8 _)     = 1
          sz (R8 _)     = 1
          supported (I8 _) = True
          supported (R8 _) = True
          supported (I32 _) = True
          supported (R64 _) = True
          supported _ = False
ppush v = error $ "ppush doesn't support " ++ show v

-- Push a string on the stack
ppushStr :: String -> Lang ()
ppushStr s = do 
    mapM ppush $ map (I8 . ascii) s
    return ()

ppushI32 :: (Integral a) => a -> Lang ()
ppushI32 n = do
    ppush $ I32 $ fromIntegral n

-- A function made up of assembly which has a type.
-- This is used to define the built-in (basic) functions
-- No type checking is performed on the body!
defFunBasic :: String
            -> Type
            -> Lang ()
            -> Lang ()
defFunBasic funName funTy funBody = do
    envAddType funName funTy
    x86 $ asm $ setLabel funName
    funBody
    x86 $ asm $ setLabel (funName ++ "_return")
    x86 $ ret

-- A helper function to jump to the return point of the function.
returnFunc funName = do
    x86 $ jmpLabel $ funName ++ "_return"

runLang :: Lang a 
        -> X86_64 (a, LangState)
runLang stm = runStateT stm initLangState
