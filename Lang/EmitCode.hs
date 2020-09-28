module Lang.EmitCode where

import X86.Datatypes
import X86.X86
import ASM.ASM
import ASM.Datatypes

import Data.Word

import Lang.BasicFunctions
import Lang.Debug
import Lang.Linux

defineEmitFunctions = do
    doc "------------------------------------------------"
    doc "Kernel  functions  that  allow us to bootstrap a"
    doc "language that defines its own emit functionality"
    doc "------------------------------------------------"
    defineEmitIfStart
    defineEmitIfStart
    defineEmitIfEnd
    defineEmitRet
    defineEmitCall
    defineEmitPPushW64
    defineEmitPPushW8 

    -- Allowing these leads to a self-extensible interpreter,
    -- which allows for rapid prototyping
    defineEmitW8
    defineEmitW64

defineEmitIfStart :: X86_64()
defineEmitIfStart = defFunBasic "emit_if_start" body
  where
    body = do
        doc "Emit IF start"
        -- ppop rax
        -- 48 8B 06 : mov rax,[rsi]
        mov (derefOffset r9 0) (I8 0x48)
        mov (derefOffset r9 1) (I8 0x8B)
        mov (derefOffset r9 2) (I8 0x06)
        add r9 (I32 3)

        -- 48 3D 00 00 00 00 : cmp rax,0x0
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
        doc "out later (in emit_if_end), save the position at which the "
        doc "address is to be written."
        doc "The offset is to be calculated from the position of JE (0F 84)"
        mov (derefOffset r9 0) (I8 0x0F)
        mov (derefOffset r9 1) (I8 0x84)
        mov (derefOffset r9 2) (I8 0x00)
        mov (derefOffset r9 3) (I8 0x00)
        mov (derefOffset r9 4) (I8 0x00)
        mov (derefOffset r9 5) (I8 0x00)
        add r9 (I32 6)

        ppush r9

defineEmitIfEnd :: X86_64 ()
defineEmitIfEnd = defFunBasic "emit_if_end" body
  where
    body = do
        doc "Emit IF end (it doesn't emit an opcode, it just writes the"
        doc "current r9 at the appropriate location, where the 'if' began):"

        ppop rax              -- Retrieve the saved R9 from the stack;
                              -- rbx := Offset =
        mov rbx r9        -- Current offset R9
        sub rbx rax       -- Minus saved offset R9
        
        -- TODO: simplify - how do we copy from 32 bit register eax 4 bytes 
        -- in 1 shot?
        mov (derefOffset rax (-4)) bl
        sar rbx (I8 8)
        mov (derefOffset rax (-2)) bl
        sar rbx (I8 8)
        mov (derefOffset rax (-3)) bl
        sar rbx (I8 8)
        mov (derefOffset rax (-1)) bl


defineEmitPPushW8 :: X86_64 ()
defineEmitPPushW8 = defFunBasic "emit_ppush_w8" body
  where
    body = do
        doc "Emit assembly code in the dynamic code area"
        doc "That pushes the 8bit literal from the stack onto the stack."
        cpush rax
        xor rax rax
        ppopW8 al
        doc "X86: sub RSI 1"
        mov (derefOffset r9 0) (I8 0x48)
        mov (derefOffset r9 1) (I8 0x81)
        mov (derefOffset r9 2) (I8 0xEE)
        mov (derefOffset r9 3) (I8 0x01)
        mov (derefOffset r9 4) (I8 0x00)
        mov (derefOffset r9 5) (I8 0x00)
        mov (derefOffset r9 6) (I8 0x00)
        add r9 (I32 7)

        doc "X86: mov [RSI+0] <- IMM8"
        doc "C6 46 00 AB"
        mov (derefOffset r9 0) (I8 0xC6)
        mov (derefOffset r9 1) (I8 0x46)
        mov (derefOffset r9 2) (I8 0x00)
        mov (derefOffset r9 3) al
        add r9 (I32 4)

        cpop rax

defineEmitPPushW64 :: X86_64 ()
defineEmitPPushW64 = defFunBasic funName body
  where
    funName = "emit_ppush_w64"
    body    = do
        doc "Emit assembly code in the dynamic code area"
        doc "That pushes a 64bit literal value on the stack."
        doc "Backup rax"
        cpush rax
        xor rax rax
        doc "Take the 64 bit literal that we need to push and place it in rax"
        ppop rax
        -- TODO: Improve code (repeated code)
        doc "X86: sub RSI 8"
        do
            mov (derefOffset r9 0) (I8 0x48)
            mov (derefOffset r9 1) (I8 0x81)
            mov (derefOffset r9 2) (I8 0xEE)
            mov (derefOffset r9 3) (I8 0x08)
            mov (derefOffset r9 4) (I8 0x00)
            mov (derefOffset r9 5) (I8 0x00)
            mov (derefOffset r9 6) (I8 0x00)
            add r9 (I32 7)

        doc "We want to load a whole imm64 value but the intel"
        doc "manual doesn't say how to do that. We do two dword movs."

        doc "emit mov dword [rsi+0] <- imm32"
        doc "(c7 06 imm32)"
        doc "then"
        doc "emit mov dword [rsi+4] <- imm32"
        doc "(c7 46 04 imm32)"
        do
            mov (derefOffset r9 0) (I8 0xC7)
            mov (derefOffset r9 1) (I8 0x06)
            mov (derefOffset r9 2) al
            sar rax (I8 8)
            mov (derefOffset r9 3) al
            sar rax (I8 8)
            mov (derefOffset r9 4) al
            sar rax (I8 8)
            mov (derefOffset r9 5) al
            sar rax (I8 8)
            doc "Emitted 6 bytes"
            add r9 (I32 6)
        do
            mov (derefOffset r9 0) (I8 0xC7)
            mov (derefOffset r9 1) (I8 0x46)
            mov (derefOffset r9 2) (I8 0x04)
            mov (derefOffset r9 3) al
            sar rax (I8 8)
            mov (derefOffset r9 4) al
            sar rax (I8 8)
            mov (derefOffset r9 5) al
            sar rax (I8 8)
            mov (derefOffset r9 6) al
            sar rax (I8 8)
            doc "Emitted 7 bytes"
            add r9 (I32 7)

        {-
        do
            mov (derefOffset r9 0) (I8 0x48)
            mov (derefOffset r9 1) (I8 0xC7)
            mov (derefOffset r9 2) (I8 0x46)
            mov (derefOffset r9 3) (I8 0x00)
            -- TODO: Could we copy from 32 bit register eax 4 bytes directly?
            mov (derefOffset r9 4) al
            sar rax (I8 8)
            mov (derefOffset r9 5) al
            sar rax (I8 8)
            mov (derefOffset r9 6) al
            sar rax (I8 8)
            mov (derefOffset r9 7) al
            sar rax (I8 8)
            add r9 (I32 8)

        doc "X86: mov [RSI+4] <- IMM32"
        do
            mov (derefOffset r9 0) (I8 0x48)
            mov (derefOffset r9 1) (I8 0xC7)
            mov (derefOffset r9 2) (I8 0x46)
            mov (derefOffset r9 3) (I8 0x04)
            -- TODO: Could we copy from 32 bit register eax 4 bytes directly?
            mov (derefOffset r9 4) al
            sar rax (I8 8)
            mov (derefOffset r9 5) al
            sar rax (I8 8)
            mov (derefOffset r9 6) al
            sar rax (I8 8)
            mov (derefOffset r9 7) al
            add r9 (I32 8)

        -}
        doc "Restore rax"
        cpop rax

defineEmitW8 :: X86_64 ()
defineEmitW8 = defFunBasic "emit_w8" body 
  where
    body = do
        doc "Takes the w8 value from the top of the stack and emits it"
        doc "in the JIT code generation area. Uses RAX"
        -- cpush rax
        -- xor rax rax

        -- writeMsgHelper ":: emit "
        -- callLabel "dbg_dump_ptop_w8"

        ppop al
        mov (derefOffset r9 0) al
        inc r9
        -- cpop rax
 
defineEmitW64 :: X86_64 ()
defineEmitW64 = defFunBasic "emit_w64" body
  where
    body = do
        -- writeMsgHelper ":: emit "
        -- callLabel "dbg_dump_ptop_w64"
        ppop rax
        mov (derefOffset r9 0) rax
        add r9 (I32 8)
        

defineEmitRet :: X86_64 ()
defineEmitRet = defFunBasic funName body
  where
    funName = "emit_ret"
    body = do
        mov (derefOffset r9 0) (I8 0xC3)
        inc r9

defineEmitCall :: X86_64 () 
defineEmitCall = defFunBasic "emit_call" body
  where
    body = do
        doc "Emit a call to a certain dictionary entry, the address of which"
        doc "is on the stack."

        cpush rax
        ppop rax
        mov rax (derefOffset rax 16)

        -- mov rax <addr>
        do
            mov (derefOffset r9 0) (I8 0x48)
            mov (derefOffset r9 1) (I8 0xB8)
            mov (derefOffset r9 2) rax
            add r9 (I32 10)

        -- call rax
        do
            mov (derefOffset r9 0) (I8 0xFF)
            mov (derefOffset r9 1) (I8 0xD0)
            add r9 (I32 2)
        cpop rax 
