{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Lang.Lang where

import ASM.Datatypes
import ASM.ASM

import X86.Datatypes 
import X86.X86

import Lang.Linux
import Lang.Debug
import Lang.BasicFunctions
import Lang.Parsing

def :: String -> X86_64 ()
def defName = do
    doc $ "ASM Dictionary entry for " ++ defName
    doc "ASM or term sequence address in memory:"
    let hash = I64 $ fnv1 $ map ascii defName
    doc "Backup the old r11"
    mov rax r11

    doc "Allocate a new record"
    add r11 $ I32 24
    
    doc "Set values of the new record"
    mov (derefOffset r11 0)  rax -- prev dict. entry
    mov rax $ hash
    mov (derefOffset r11 8)  rax -- hash
    mov rax                 (L64 defName)
    mov (derefOffset r11 16) rax  -- code address (by label ref.)

baseDefEntries = do
    doc "Populate the dictionary with base functions."
    doc "Keep it a linked list rather than an array because there "
    doc "might be variable-length records in the future (e.g. keep the "
    doc "entry name ASCII representation)"
    doc "Simple Dictionary of cells [ prev ][ name ][ addr ]"
    doc "See also: initDictionaryMemLinux "

    -- def "repl"
    def "dbg_dump_dictionary"
    def "dbg_dump_ptop_w8"
    def "dbg_dump_ptop_w64"

    def "read_head_w8"
    def "parse_identifier"
    -- def "parse_numeric"
    -- def "parse_if_term"
    def "not"
    def "dec"
    -- def "pdrop_w64"
    -- def "pdrop_w8"
    -- def "ppeer_w8"
    -- def "ppeer_w64"
    def "dup"
    def "eq"
    def "lt"
    def "lte"
    def "gt"
    def "gte"
    def "minus"
    def "times"
    def "and"
    def "exit"
    def "plus"
    def "push1"
    def "write_char_w8"
    def "write_char_w64"
    -- def "read_char_w8"

baseDefBodies = do
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

    -- defineRdChrW8Linux
    defineWrCharW8Linux
    defineWrChrW64Linux

    Lang.Parsing.defineRdHeadW8
    -- Lang.Parsing.defineRdTailW8
    -- Lang.Parsing.defineRdTailAZW8
    Lang.Parsing.defineParseIdentifier
    Lang.Parsing.defineParseAZ09_
    Lang.Parsing.defineParseWhitespace
    Lang.Parsing.defineParseWhitespaceSeq
    Lang.Parsing.defineParse09
    Lang.Parsing.defineParseInteger
    -- defineREPL
    defineTermLook
    defineTermHash
    defineTermLookNohash

    definePush1
    definePushK

    defineDUP
    defineNOT
    defineDEC

    Lang.Debug.defineDbgDumpDictionary
    Lang.Debug.defineDbgDumpPtop64
    Lang.Debug.defineDbgDumpPtop8

    defineExit

definePush1 = defFunBasic "push1" body where
    body = do
        ppush $ I32 1

definePushK = defFunBasic "pushk" body where
    body = do
        ppush $ I32 75

defineDUP :: X86_64 () 
defineDUP = defFunBasic "dup" body
  where
    body    = do
        doc "Duplicate the top item on the stack."
        ppeek rax
        ppush rax
        ret

defineDEC :: X86_64 ()
defineDEC = defFunBasic "dec" body
  where
    body    = do
        doc "Decrement the stack top."
        ppop rax
        dec rax
        ppush rax

defineNOT :: X86_64 ()
defineNOT = defFunBasic funName body
  where
    funName = "not"
    ty      = undefined
    body    = do
        doc "If the top of the stack is 0, replace with 1. "
        doc "Otherwise replace with 0."
        ppop rax
        cmp rax $ I32 0
        jeNear "NOT_PUSH_0"
        ppush $ I32 1
        ret
        asm $ setLabel "NOT_PUSH_0"
        ppush $ I32 0
        ret

-- Type :w64       -> :
-- Func :exit_code -> :
-- Exits the program with a specified exit code.
defineExit :: X86_64 ()
defineExit = defFunBasic "exit" body
    where
        body = do
            ppop rbx
            mov rax $ I64 1
            int

-- Type : -> :w64:w64
-- Func : -> :term_address:success_or_failure
defineTermLook :: X86_64 ()
defineTermLook = defFunBasic funName body
  where
    funName = "TERM_LOOK"
    body    = do
        doc "Lookup a term in the dictionary."
        doc "Compute the term hash."
        callLabel "TERM_HASH"
        doc "Traverse the dictionary using rax until we find"
        doc "either the emtpy dictionary or the term."
        doc ""
        doc "rax now holds the hash we're looking for."

        callLabel "TERM_LOOK_NOHASH"

defineTermLookNohash :: X86_64 ()
defineTermLookNohash = defFunBasic funName body
  where
    funName = "TERM_LOOK_NOHASH"
    body    = do
        doc "Lookup a term in the dictionary."
        doc "Use the hash given on the stack."
        doc "Traverse the dictionary using rax until we find"
        doc "either the emtpy dictionary or the term."
        ppop rax 
        doc "rax holds the hash we're looking for."
        mov rbx r11 -- Begin from the dictionary top.
        asm $ setLabel "TERM_LOOK_WHILE"
        do
            cmp rbx $ I32 0
            jeNear "TERM_LOOK_NOT_FOUND"

            doc "Take the current dictionary entry hash: def.hash"
            mov rcx (derefOffset rbx 8)
            cmp rcx rax
            jeNear "TERM_LOOK_FOUND"
            
            doc "Advance the dictionary to the following item (def.prev)"
            mov rbx (derefOffset rbx 0) 

            jmpLabel "TERM_LOOK_WHILE"
        asm $ setLabel "TERM_LOOK_FOUND"
        doc "Found! Return the matching dictionary address"
        ppush rbx 
        ppush $ I32 1
        ret

        asm $ setLabel "TERM_LOOK_NOT_FOUND"
        doc "Not found! indicate that there is an error."
        ppush $ I32 0
        ppush $ I32 0
        ret

defineWrCharW8Linux :: X86_64 ()
defineWrCharW8Linux = defFunBasic "write_char_w8" body
  where 
    body = do
        mov rdx $ I64 1 -- How many bytes to write?
        mov rax $ I64 $ fromIntegral linux_sys_write
        mov rbx $ I64 $ fromIntegral linux_stdout 
        -- read chars from the top of the params stack, i.e. from our buffer.
        mov rcx rsi
        int
        pdropW8 1 -- Drop the top of the stack which was the char
                  -- we've just printed.
        ppush rax -- The putchar result, how many bytes we've written

-- http://man7.org/linux/man-pages/man2/write.2.html
-- Type :w64 -> :int
-- Func :chr -> :success
-- Side effect, prints buf on stdout.
defineWrChrW64Linux :: X86_64 ()
defineWrChrW64Linux = defFunBasic "write_char_w64" body
  where 
    body = do
        mov rdx $ I64 1 -- How many bytes to write?
        mov rax $ I64 $ fromIntegral linux_sys_write
        mov rbx $ I64 $ fromIntegral linux_stdout 
        -- read chars from the top of the params stack, i.e. from our buffer.
        mov rcx rsi
        int
        pdrop 1   -- Drop the top of the stack which was the char
                  -- we've just printed.
        ppush rax -- The putchar result, how many bytes we've written



-- Type:      :w64:w64 -> w64
-- Operation: :a  :b   -> a (x) b
-- defineBinop :: String -> X86_64()
-- FIXME: Think about / introduce IMUL/IPLUS etc for signed/unsigned values!
-- We might need overflow checking also, i.e. return success or failure.
defineBinop fn op
    | fn == "plus" || fn == "minus" || fn == "and" =
    defFunBasic fn body where
        body = do
            ppop rax
            ppop rbx
            op rax rbx
            ppush rax -- Result
defineBinop fn _ 
    | fn == "times" = do
    defFunBasic fn body where
        body = do
            ppop rax
            ppop rbx
            mul rbx  -- rax <- rax * rbx
            ppush rax -- Result

defineCMP :: String -> (String -> X86_64()) -> X86_64 ()
defineCMP funName jmpCmpFun = defFunBasic funName funBody
  where
    funBody = do
        ppop rax
        ppop rbx
        cmp rax rbx -- test: rax (?) rbx
        jmpCmpFun trueLabel -- if cmpFun holds, jump to trueLabel
        ppush (I32 0)
        ret
        asm $ setLabel trueLabel
        ppush (I32 1)
        ret
    trueLabel = funName ++ "_true"

-- Type:      :w64:w64 -> :w64
-- Operation: :a  :b   -> :a<<b
defineShiftLeft = defFunBasic fn body where
    fn   = "shift_left"
    body = do 
        ppop rcx -- shift
        ppop rax -- number
        sal rax cl
        ppush rax

-- Type:      :w64:w64 -> :w64
-- Operation: :a  :b   -> :a<<b
defineShiftRight = defFunBasic fn body where
    fn   = "shift_right"
    body = do 
        ppop rcx -- shift
        ppop rax -- number
        sar rax cl
        ppush rax

-- Improve: Is TESH_HASH expressible as a sequence of more basic words?
-- ( c0..cn-1:n -- hash )
-- alters: rbx, rcx, rax, rdx
defineTermHash :: X86_64 ()
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
        mov rax $ I64 fnvOffsetBasis

        asm $ setLabel "TERM_HASH_WHILE"
        do 
            cmp rbx $ I32 0
            jeNear "TERM_HASH_BREAK"
            mov rcx (I64 fnvPrime)
            mul rcx
            xor rcx rcx -- Zeroing rcx is necessary
            ppopW8 cl
            xor rax rcx
            dec rbx
            jmpLabel "TERM_HASH_WHILE"
        asm $ setLabel "TERM_HASH_BREAK"

        ppush rax
        ret


defineREPL :: X86_64 ()
defineREPL = defFunBasic "repl" body
  where
    body = do
        doc "REPL"
        asm $ setLabel "REPL_START"

        doc "Read a term:"
        callLabel "READ_PRINTABLES_W8"
        assertPtop 1 "READ_PRINTABLES_W8 returned an error!"
        pdrop 1
        doc "Hash the term we've read:"
        callLabel "TERM_HASH"

        -- The first word is the operation we wish to make.
        -- There are two operations for now, define and call.
        doc "Switch on a different functionality"
        doc "depending on the hash of the term we've just read:"

        ppop rax
        mov rbx (I64 $ fnv1s "def")
        cmp rax rbx
        jeNear "REPL_DEF_CALL"
        mov rbx (I64 $ fnv1s "run")
        cmp rax rbx
        jeNear "REPL_RUN"
        mov rbx (I64 $ fnv1s "q")
        cmp rax rbx
        jeNear "REPL_QUIT"
        
        writeMsgHelper "Unknown command! (expected: def/run/q)\n"
        jmpLabel "REPL_START"

        asm $ setLabel "REPL_DEF_CALL"
        callLabel "REPL_DEF"
        jmpLabel "REPL_START" -- After the definition, resume from the 
                                    -- beginning.
        asm $ setLabel "REPL_RUN"
        callLabel "READ_PRINTABLES_W8"
        assertPtop 1 "Could not read definition term"
        pdrop 1
        callLabel "TERM_LOOK"
        -- Check for an unknown term (do nothing in that case).
        ppop rax
        cmp rax (I32 0)
        jeNear "REPL_RUN_UNDEFINED"
        
        -- Now take the '.addr' field from the dictionary term found by LOOK:
        ppop rax
        mov rax (derefOffset rax 16)
        call rax

        writeMsgHelper "Run term done.\n"
        jmpLabel "REPL_START"

        asm $ setLabel "REPL_RUN_UNDEFINED"
        assertPtop 0 "TERM_LOOK failed so the result should be 0."
        pdrop 1 -- Drop the 0 From TERM_LOOK
        writeMsgHelper "L.1158 Undefined term!\n"
        jmpLabel "REPL_START"

        asm $ setLabel "REPL_QUIT"
 
