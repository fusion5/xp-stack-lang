module Lang.Debug where

import ASM.Datatypes
import ASM.ASM

import X86.Datatypes
import X86.X86

import Lang.Linux
import Lang.BasicFunctions

import Data.Int

import Control.Monad (replicateM)

-- Provides simple instrumentation for testing hypotheses / predicates
-- in programs. Only to be used for debugging.

assertGeneric :: Integer -> String -> X86_64 () -> X86_64 ()
assertGeneric n msg raxGet = do
    -- Save the value of rax and rbx to the call stack since this function
    -- uses them.
    push rax
    push rbx
    -- Test our predicate.
    raxGet 
    mov rbx $ I64 $ fromIntegral n
    cmp rax rbx
    assertPass <- asm $ freshLabelWithPrefix "assert_pass_"
    jeNear assertPass -- overflow possibility avoidance
    -- The assert has failed...
    -- Exit loudly.
    -- push rax

    writeMsgHelper "Expected: 0x"
    ppush rbx
    callLabel "dbg_dump_ptop_w64"
    pdrop 1

    writeMsgHelper "Actual:   0x"
    ppush rax
    callLabel "dbg_dump_ptop_w64"
    pdrop 1
 
    -- Write the standard assert failed error to stdout.
    mov rax $ I64 $ fromIntegral $ linux_sys_write
    mov rbx $ I64 $ fromIntegral $ linux_stdout
    mov rdx $ I64 $ fromIntegral $ length assertFailedMsg
    asm $ addString assertFailedMsg
    mov rcx (S64 assertFailedMsg)
    int
    
    -- Write the custom assert fail error message to stdout.
    -- Note that a too long message could damage our je assertPass.
    -- assertMSGStart <- freshLabelWithPrefix "assert_fail_msg_"
    -- assertExit <- asm $ freshLabelWithPrefix "assert_exit_"

    let msgWithNL = msg ++ "\n"
    mov rax $ I64 $ fromIntegral $ linux_sys_write
    mov rbx $ I64 $ fromIntegral $ linux_stdout
    mov rdx $ I64 $ fromIntegral $ length msgWithNL
    -- movLabelRef64 rcx assertMSGStart
    asm $ addString msgWithNL -- Add to strings table
    mov rcx (S64 msgWithNL)   -- Set rcx to the string table reference of msg
    int

    -- TODO: Dump call stack?
 
    pop rbx     -- Restore rbx (actually not needed since we are exiting)
    pop rax     -- Restore rax (actually not needed since we are exiting)
    mov rbx rax -- Return the actual value that we got.
    mov rax $ I64 $ fromIntegral linux_sys_exit
    int
    
    setLabel assertPass
    pop rbx     -- Restore rbx
    pop rax     -- Restore rax
  where
    assertFailedMsg = "Assert failed: "
   
assertCtop :: Integer -> String -> X86_64 ()
assertCtop n msg = 
    assertGeneric n msg raxSet
  where
    raxSet = mov rax $ rsp `derefOffset` 0 -- Peek the call stack top

{-
assertPpop :: Integer -> String -> X86_64 ()
assertPpop n msg = do
    assertPtop n msg
    pdrop 1
-}

assertPtop :: Integer -> String -> X86_64 ()
assertPtop n msg = 
    assertGeneric n msg raxSet
  where
    raxSet = mov rax $ rsi `derefOffset` 0 -- Peek the param stack top

assertPtopW8 :: Integer -> String -> X86_64 ()
assertPtopW8 n msg = 
    assertGeneric n msg raxSet
  where
    raxSet = mov al $ rsi `derefOffset` 0 -- Peek the param stack top

defineDbgDumpDictionary :: X86_64()
defineDbgDumpDictionary = defFunBasic "dbg_dump_dictionary" body
  where
    body = do
        doc "Dump dictionary"
        doc "Backup registers"
        ppush rbx
        ppush rcx

        writeMsgHelper "Dictionary\n"
        writeMsgHelper "----------\n"
        mov rbx r11
        asm $ setLabel "DBG_DUMP_DICTIONARY_LOOP"
        do
            cmp rbx $ I32 0
            jeNear "DBG_DUMP_DICTIONARY_END"

            mov rcx (derefOffset rbx 8)
            ppush rcx
            callBody "dbg_dump_ptop_w64"
            pdrop 1

            doc "Advance the dictionary to the following item (def.prev)"
            mov rbx (derefOffset rbx 0) 

            jmpLabel "DBG_DUMP_DICTIONARY_LOOP"
        setLabel "DBG_DUMP_DICTIONARY_END"

        writeMsgHelper "----------\n"
        doc "Restore register backups"
        ppop rcx
        ppop rbx

-- Type :w64 -> :w64
-- Func :val -> :val
-- Does nothing; as a side effect, it prints the top of the stack on stdout.
defineDbgDumpPtop64 :: X86_64 ()
defineDbgDumpPtop64 = defFunBasic "dbg_dump_ptop_w64" body
  where
    body = do
        doc "Backup the registers used so as not to affect the caller"
        doc "rax holds the top of the stack."
        ppush rax
        ppeer 1 rax
        ppush rbx
        ppush rcx
        ppush rdx

        doc "A 64 bit value needs 16 chars to be printed. RCX is the counter."
        mov rcx $ I64 16 

        asm $ setLabel "DBG_DUMP_PTOP_64_START"
        mov rbx $ I64 0x10
        xor rdx rdx
        cmp rcx $ I32 0
        je "DBG_DUMP_PTOP_64_END"
        div_ rbx
        doc "Print rdx, which holds the remainder."

        cmp rdx $ I32 10
        jl "DBG_DUMP_PTOP_64_LT10"
        doc "Value between [10 and 15]"
        doc "Add 55 to rdx"
        add rdx $ I32 55
        ppush rdx
        dec rcx
        jmpLabel "DBG_DUMP_PTOP_64_START"

        asm $ setLabel "DBG_DUMP_PTOP_64_LT10"
        doc "Value between [0 and 9]"
        doc "Add 0x30 to rdx"
        add rdx $ I32 0x30
        ppush rdx
        dec rcx
        jmpLabel "DBG_DUMP_PTOP_64_START"

        asm $ setLabel "DBG_DUMP_PTOP_64_END"
        doc "16 times write char. Conveniently, using the stack before "
        doc "printing reverses the "
        doc "order of the chars (which we need to do)."
        replicateM 16 $ do
            callLabel "write_char_w64"
            pdrop 1

        doc "Write a newline char"
        ppush $ I32 0x0A
        callLabel "write_char_w64"
        pdrop 1

        doc "Restore registers from the backup so as not to affect the caller"
        ppop rdx
        ppop rcx
        ppop rbx
        ppop rax

-- Type :w8  -> :w8
-- Func :val -> :val
-- Does nothing; as a side effect, it prints the top of the stack on stdout.
defineDbgDumpPtop8 :: X86_64 ()
defineDbgDumpPtop8 = defFunBasic "dbg_dump_ptop_w8" body
  where
    body = do
        doc "Backup the registers used so as not to affect the caller"
        doc "rax holds the top of the stack."
        -- TODO: Use instructions that back up/restore all registers (just in 
        -- case)
        ppush rax
        ppeer 1 rax
        ppush rbx
        ppush rcx
        ppush rdx
        doc "rax holds the top of the stack."

        doc "A 8 bit value needs 2 chars to be printed. RCX is the counter."
        mov rcx $ I64 2

        asm $ setLabel "DBG_DUMP_PTOP_8_START"
        mov rbx $ I64 0x10
        xor rdx rdx
        cmp rcx $ I32 0
        je "DBG_DUMP_PTOP_8_END"
        div_ rbx
        doc "Print rdx, which holds the remainder."

        cmp rdx $ I32 10
        jl "DBG_DUMP_PTOP_8_LT10"
        doc "Value between [10 and 15]"
        doc "Add 55 to rdx"
        add rdx $ I32 55
        ppush rdx
        dec rcx
        jmpLabel "DBG_DUMP_PTOP_8_START"

        asm $ setLabel "DBG_DUMP_PTOP_8_LT10"
        doc "Value between [0 and 9]"
        doc "Add 0x30 to rdx"
        add rdx $ I32 0x30
        ppush rdx
        dec rcx
        jmpLabel "DBG_DUMP_PTOP_8_START"

        asm $ setLabel "DBG_DUMP_PTOP_8_END"
        doc "2 times write char. Conveniently, using the stack before "
        doc "printing reverses the "
        doc "order of the chars (which we need to do)."
        replicateM 2 $ do
            callLabel "write_char_w64"
            pdrop 1

        doc "Write a newline char"
        ppush $ I32 0x0A
        callLabel "write_char_w64"
        pdrop 1

        doc "Restore all backup registers so as not to affect the caller"
        ppop rdx
        ppop rcx
        ppop rbx
        ppop rax

