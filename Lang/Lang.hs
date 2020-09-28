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
import Lang.EmitCode

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
    doc "Populate the dictionary with some base functions from the assembly."
    doc "The dictionary is kept as a linked list rather than an array because "
    doc "there might be variable-length records in the future (e.g. the "
    doc "entry name ASCII representation)."
    doc "Simple Dictionary of cells [ prev ][ name ][ addr ]"
    doc "See also: initDictionaryMemLinux "

    def "repl"
    def "dbg_dump_dictionary"
    def "dbg_dump_ptop_w8"
    def "dbg_dump_ptop_w64"

    def "new_def"
    def "emit_w8"
    def "emit_w64"
    def "emit_ppush_w8"  -- Convience dictionary export 
    def "emit_ppush_w64" -- Convience dictionary export

    -- def "read_head_w8"
    -- def "parse_identifier"
    -- def "parse_numeric"
    -- def "parse_if_term"
    def "not"
    def "dec"
    def "drop_w64" -- Not very memory safe but for experiments...
    def "drop_w8"
    -- def "ppeer_w8"
    -- def "ppeer_w64"
    def "dup_w64"
    def "dup_w8"
    def "eq_w64"
    def "eq_w8"
    def "lt_w64"
    def "lt_w8"
    def "gt_w64"
    def "gt_w8"
    def "lte_w64"
    def "lte_w8"
    def "gte_w64"
    def "gte_w8"
    def "minus"
    def "times"
    def "and"
    def "exit"
    def "plus"
    def "push1"
    def "write_char_w8"
    def "write_char_w64"

    -- def "emit_lit_64"
    def "term_hash"
    -- def "read_char_w8"

baseDefBodies = do
    defineCMPW64 "eq_w64"  je
    defineCMPW64 "lt_w64"  jl
    defineCMPW64 "gt_w64"  jg
    defineCMPW64 "lte_w64" jle
    defineCMPW64 "gte_w64" jge

    defineCMPW8 "eq_w8"  je
    defineCMPW8 "lt_w8"  jl
    defineCMPW8 "gt_w8"  jg
    defineCMPW8 "lte_w8" jle
    defineCMPW8 "gte_w8" jge

    defineBinop "plus"  add
    defineBinop "minus" sub
    defineBinop "times" undefined
    defineBinop "and"   and_

    defineShiftLeft
    defineShiftRight

    -- defineRdChrW8Linux
    defineWrCharW8Linux
    defineWrChrW64Linux

    Lang.Parsing.defineParsers
    Lang.EmitCode.defineEmitFunctions
    
    defineNewDef
    
    defineREPL
    defineTermLook
    defineTermHash
    defineTermLookNohash

    definePush1
    definePushK

    defineDropW64
    defineDropW8

    defineDUPW64
    defineDUPW8

    defineNOT
    defineDEC

    Lang.Debug.defineDbgDumpDictionary
    Lang.Debug.defineDbgDumpPtop64
    Lang.Debug.defineDbgDumpPtop8

    defineExit

definePush1 = defFunBasic "push1" body where
    body = do
        ppush $ I32 1

defineDropW64 = defFunBasic "drop_w64" body where
    body = pdrop 1

defineDropW8 = defFunBasic "drop_w8" body where
    body = pdropW8 1

definePushK = defFunBasic "pushk" body where
    body = do
        ppush $ I32 75

defineDUPW64 :: X86_64 () 
defineDUPW64 = defFunBasic "dup_w64" body
  where
    body = do
        doc "Duplicate the top item on the stack."
        ppeek rax
        ppush rax
        ret

defineDUPW8 :: X86_64 () 
defineDUPW8 = defFunBasic "dup_w8" body
  where
    body = do
        doc "Duplicate the top item on the stack."
        ppeek al
        ppush al
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

defineTermHash :: X86_64 ()
defineTermHash = defFunBasic "term_hash" body
  where
    body    = do
        doc "Backup RBX, RAX, RCX since we'll be using them"
        cpush rbx
        cpush rax
        cpush rcx
        doc "Given a word on the stack, compute its hash. This helps to"
        doc "compare words for equality when searching the dictionary."
        doc "Trying out the (simple) FNV Hash function from Wikipedia"
        doc "RBX holds the length to iterate."
        doc "E.g. 'cba' has hash 15626587013303479755"

        -- writeMsgHelper ":: hash length: "
        -- callLabel "dbg_dump_ptop_w64"
        
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

            -- writeMsgHelper ":: hash char: "
            -- callLabel "dbg_dump_ptop_w8"

            ppopW8 cl
            xor rax rcx
            dec rbx
            jmpLabel "TERM_HASH_WHILE"
        asm $ setLabel "TERM_HASH_BREAK"

        ppush rax

        -- writeMsgHelper ":: final hash: "
        -- callLabel "dbg_dump_ptop_w64"
    
        doc "Restore RBX, RAX, RCX"
        cpop rcx
        cpop rax
        cpop rbx
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
    funName = "term_hash_look"
    body    = do
        doc "Lookup a term in the dictionary."
        doc "Compute the term hash."
        callLabel "term_hash"
        doc "Traverse the dictionary using rax until we find"
        doc "either the emtpy dictionary or the term."
        doc ""
        doc "rax now holds the hash we're looking for."

        callLabel "term_look"

defineTermLookNohash :: X86_64 ()
defineTermLookNohash = defFunBasic funName body
  where
    funName = "term_look"
    body    = do
        doc "Lookup a term hash in the dictionary."
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
        -- ppush $ I32 0
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

defineCMPW8 :: String -> (String -> X86_64()) -> X86_64 ()
defineCMPW8 funName jmpCmpFun = defFunBasic funName funBody
  where
    funBody = do
        xor rax rax
        xor rbx rbx
        ppop al
        ppop bl
        cmp  rax rbx -- test: rax (?) rbx
        jmpCmpFun trueLabel -- if cmpFun holds, jump to trueLabel
        ppush (I32 0)
        ret
        asm $ setLabel trueLabel
        ppush (I32 1)
        ret
    trueLabel = funName ++ "_true"

defineCMPW64 :: String -> (String -> X86_64()) -> X86_64 ()
defineCMPW64 funName jmpCmpFun = defFunBasic funName funBody
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

-- Input: a string (length and chars). The string is consumed.
-- A new dictionary entry is created for that string.
defineNewDef = defFunBasic "new_def" body where
    body = do
        doc "Build a [ prev ][ name hash ][ addr ] record on the stack"
        doc "for a new dictionary definition."

        doc "At this point the stack contains the term parsed by "
        doc "parse_identifier. Hash it and save the hash in rax."

        {-
        writeMsgHelper "new def len:\n"
        callLabel "dbg_dump_ptop_w64"
        ppop rax
        writeMsgHelper "new def letter:\n"
        callLabel "dbg_dump_ptop_w8"
        ppush rax
        -}

        callBody "term_hash"
        -- writeMsgHelper "new term hash:\n"
        -- callLabel "dbg_dump_ptop_w64"
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
        doc "Done!"

defineREPL :: X86_64 ()
defineREPL = defFunBasic "repl" body
  where
    body = do
        doc "REPL"
        asm $ setLabel "REPL_START"

        doc "Await some input to be available"
        callLabel "read_head_w8"
        pdrop 1

        doc "Consume any whitespace and prepare the first character"
        callOptionalParser "parse_wss" "REPL_ERR_UNKNOWN_INPUT"

        doc "Read a term using parse_identifier:"
        -- TODO: Rename 'identifier' into 'term' in the parser!
        callRequiredParser "parse_identifier" "REPL_ERR_UNKNOWN_INPUT"

        doc "Hash the term we've read:"

        callLabel "term_hash"

        -- The first word is the operation we wish to make.
        -- There are two operations for now, define and call.
        doc "Switch to a different functionality"
        doc "depending on the hash of the term we've just read:"

        ppop rax
        mov rbx (I64 $ fnv1s "def")
        cmp rax rbx
        jeNear "REPL_DEF"
        mov rbx (I64 $ fnv1s "run")
        cmp rax rbx
        jeNear "REPL_RUN"
        mov rbx (I64 $ fnv1s "q")
        cmp rax rbx
        jeNear "REPL_QUIT"

        asm $ setLabel "REPL_ERR_UNKNOWN_INPUT"
        writeMsgHelper "Unknown command! (expected: def/run/q)\n"
        jmpLabel "REPL_START"

        -------------------------
        -------------------------
        asm $ setLabel "REPL_DEF"

        doc "Consume any whitespace"
        callOptionalParser "parse_wss" "REPL_ERR_UNKNOWN_INPUT"
        callRequiredParser "parse_def" "REPL_ERR_FAILED_DEF"

        writeMsgHelper "OK, defined.\n"

        doc "After the definition resume from the beginning."
        jmpLabel "REPL_START" 
                                    
        -------------------------
        -------------------------
        asm $ setLabel "REPL_RUN"

        doc "Consume any whitespace"
        callOptionalParser "parse_wss" "REPL_ERR_UNKNOWN_INPUT"
        callRequiredParser "parse_identifier" "REPL_ERR_NOT_A_TERM"

        callLabel "term_hash_look"
        -- Check for an unknown term (do nothing in that case).
        ppop rax
        cmp rax (I32 0)
        jeNear "REPL_RUN_UNDEFINED"
        
        doc "Take the '.addr' field from the term found by term_hash_look"
        ppop rax
        mov rax (derefOffset rax 16)
        
        doc "Evaluate the found entry"
        call rax

        writeMsgHelper "Run term done.\n"
        jmpLabel "REPL_START"

        asm $ setLabel "REPL_ERR_NOT_A_TERM"
        writeMsgHelper "Not a valid term to run!\n"
        jmpLabel "REPL_START"

        asm $ setLabel "REPL_ERR_FAILED_DEF"
        writeMsgHelper "Failed to parse definition!\n"
        ppushContCharW8
        writeMsgHelper "Unable to handle char: '"
        callLabel "dbg_dump_ptop_w8"
        writeMsgHelper "'\n"
        ppopW8 al
        jmpLabel "REPL_START"


        asm $ setLabel "REPL_RUN_UNDEFINED"
        assertPtop 0 "TERM_LOOK failed so the result should be 0."
        pdrop 1 -- Drop the 0 From TERM_LOOK
        writeMsgHelper "L.1158 Undefined term!\n"
        jmpLabel "REPL_START"

        asm $ setLabel "REPL_QUIT"
 
