module Lang.Debug where

import ASM.Datatypes
import ASM.ASM

import X86.Datatypes
import X86.X86

import Lang.Datatypes
import Lang.Linux
import Lang.BasicFunctions

import Data.Int

-- Provides simple instrumentation for testing hypotheses / predicates
-- in programs. Only to be used for debugging.

assertGeneric :: Integer -> String -> X86_64 () -> X86_64 ()
assertGeneric n msg raxGet = do
    -- Save the value of rax to the call stack since this function
    -- alters it.
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
    push rax

    -- Write the standard assert failed error to stdout.
    mov rax $ I64 $ fromIntegral $ linux_sys_write
    mov rbx $ I64 $ fromIntegral $ linux_stderr
    mov rdx (I64 14) -- How many bytes to write
    asm $ addString assertFailedMsg
    mov rcx (S64 assertFailedMsg)
    int

    -- Write the custom assert fail error message to stdout.
    -- Note that a too long message could damage our je assertPass.
    -- assertMSGStart <- freshLabelWithPrefix "assert_fail_msg_"
    -- assertExit <- asm $ freshLabelWithPrefix "assert_exit_"

    mov rax $ I64 $ fromIntegral $ linux_sys_write
    mov rbx $ I64 $ fromIntegral $ linux_stderr
    mov rdx $ I64 $ fromIntegral $ length msg
    -- movLabelRef64 rcx assertMSGStart
    asm $ addString msg -- Add to strings table
    mov rcx (S64 msg)   -- Set rcx to the string table reference of msg
    int

    -- TODO: Write to screen parameters expected and gotten
   
    pop rbx     -- Restore rbx
    pop rax     -- Restore rax
    mov rbx rax -- Return the actual value that we got.
    mov rax $ I64 $ fromIntegral linux_sys_exit
    int
    
    setLabel assertPass
    pop rbx     -- Restore rbx
    pop rax     -- Restore rax
  where
    assertFailedMsg = "Assert failed: "
   
assertCtop :: Integer -> String -> Lang ()
assertCtop n msg = 
    x86 $ assertGeneric n msg raxSet
  where
    raxSet = mov rax $ rsp `derefOffset` 0 -- Peek the call stack top

ppop_assert :: Integer -> String -> Lang ()
ppop_assert n msg = do
    assertPtop n msg
    pdrop 1

assertPtop :: Integer -> String -> Lang ()
assertPtop n msg = 
    x86 $ assertGeneric n msg raxSet
  where
    raxSet = mov rax $ rsi `derefOffset` 0 -- Peek the param stack top

assertPtopW8 :: Integer -> String -> Lang ()
assertPtopW8 n msg = 
    x86 $ assertGeneric n msg raxSet
  where
    raxSet = mov al $ rsi `derefOffset` 0 -- Peek the param stack top
