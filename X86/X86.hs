module X86.X86 where

import ASM.ASM
import ASM.Datatypes
import X86.Datatypes

import Data.Word
import Data.Bits
import Data.Binary.Put (putWord64le, putWord32le)
import Data.Int

derefOffset :: Val -> Integer -> Val
derefOffset (R64 r) offset = RR64 r offset
derefOffset _ _ = error "References with Offsets can only be applied to registers"

-- Documentation at the X86 programming level (asm instructions)
docX86 = asm . documentation . ("X86: " ++)

imm64 :: Word64 -> X86_64 ()
imm64 = asm . bemit . Prelude.map SWord8 . bytes putWord64le

imm32 :: Word32 -> X86_64 ()
imm32 = asm . bemit . Prelude.map SWord8 . bytes putWord32le 

imm8 :: Word8 -> X86_64 ()
imm8  = asm . bemit . (:[]) . SWord8

emit1 :: Word8 -> X86_64 ()
emit1 = asm . bemit . (:[]) . SWord8

-- The REX value is in binary "0100WRXB" where W,R,X and B are bits.
-- Detailed here: https://wiki.osdev.org/index.php?title=X86-64_Instruction_Encoding
word_rex, word_rex_w, word_rex_b, word_rex_r, word_rex_x :: Word8
word_rex   = bit 6
word_rex_w = bit 3 -- Indicates that the instruction is promoted to 64 bits
word_rex_r = bit 2 -- Permits access to registers R8-R15 of an operand
word_rex_b = bit 0 -- Permits access to registers R8-R15 of an operand
word_rex_x = bit 1 -- ?? Not yet sure what this does.

-- REX is a prefix often used in 64 bit mode instructions.
rex   a b = emit1 $ rex_   a b
rex_w a b = emit1 $ rex_w_ a b

rex_w_ a b = word_rex_w .|. rex_ a b
rex_ mayOper1 mayOper2 = word_rex 
    .|. b mayOper1 word_rex_r
    .|. b mayOper2 word_rex_b
    where b Nothing      _ = 0
          b (Just reg) w = if isSupReg reg then w else 0

{- These modes are the first 2 bits of the ModRM Byte
 - See Intel Manual P.528, ModR/M byte definition
 - ModRM byte breakup:
 -   7  6|5    4     3|2 1 0 
 - [ MOD | Reg/Opcode | R/M ]
 - The first 2 bits of the ModRM byte indicate where to get operand(s)
 - from. They refer to the final three bits of the ModRM register, i.e.
 - the (R/M) area.
 - The meaning of these bits is explained below.
 -}
modReg, modRegRef, modRegRef8bitOffset, modRegRef32bitOffset :: Word8
{- the operand at bits 2,1,0 is the value found in the register: -}
modReg   = bit 7 .|. bit 6 
{- The operand is in memory, at the exact address found in the register
 - (dereference of the address in the register is done):
 -}
modRegRef = 0 
{- The operand is in memory, at the address found by adding the register and
 - an 8 bit displacement found in an extra word that follows the ModRM
 - byte (the offset is sign-extended and added to the index register).
 -}
modRegRef8bitOffset  = bit 6 
{- Same as modRegRef8bitOffset except the offset is given by
 - 16 bits that follow the ModRM byte.
 -}
modRegRef32bitOffset = bit 7

-- This byte specifies up to 2 registers.
-- TODO: This could be simplified, study Morse's Introduction to 8086
-- description for more insight...
mrmByte :: Val -- Register 
        -> Val -- Register or Memory
        -> Word8
mrmByte (R64 reg) (R64 rM) = 
    modReg .|. (index reg `shiftL` 3) .|. index rM

-- The bits that form a specific MRM byte opcode extension
-- bits 3, 4, 5, usually found in the Intel manual as /0, /1,
-- etc., up to /7
opcodeExt :: Integer -> Word8
opcodeExt x | 0 <= x && x <= 7 = 
    fromIntegral x `shiftL` 3

-- Loads the value from the top of the stack
-- to the location specified with the destination operand and pops
-- the stack.
pop :: Val -> X86_64()
pop (R64 to) = do
    docX86 $ "pop " ++ show to
    if isSupReg to then rex Nothing (Just to) else return ()
    emit1 $ 0x58 .|. index to
    asm $ bflush

mov :: Val -> Val -> X86_64 ()
mov to from = do
    docX86 $ "mov " ++ show to ++ " <- " ++ show from
    mov_ to from
    asm $ bflush

-- Intel manual P.1207/5038
mov_ :: Val -> Val -> X86_64 ()
-- Emit immediate data to register
mov_ (R64 dst) (I64 src) = do       -- REX.W + B8 +rd io | MOV r64, imm64
    rex_w Nothing (Just dst) 
    emit1 $ 0xB8 .|. index dst     -- Which register in the last 3 bits of 0xB8
    imm64 src
mov_ (R64 dst) (L64 label) = do
    rex_w Nothing (Just dst) 
    emit1 $ 0xB8 .|. index dst
    asm $ emitLabelRef64 label
mov_ (R64 dst) (S64 str) = do
    rex_w Nothing (Just dst) 
    emit1 $ 0xB8 .|. index dst
    asm $ emitStringRef str
mov_ o1@(R64 dst) o2@(R64 src) = do -- REX.W + 89 /r | MOV r/m64, r64
    rex_w (Just dst) (Just src)
    emit1 $ 0x89
    emit1 $ mrmByte o2 o1
mov_ o1@(R64 dst) o2@(RR64 src offset)
    -- | offset == 0 = do -- Offset = 0 could be done with a diff command
    --                                  but we do what nasm does here.
    | offset < min8BitValI || max8BitValI < offset = do 
        rex_w (Just dst) (Just src)
        emit1 $ 0x8B
        emit1 $ modRegRef32bitOffset .|. 
            (index dst `shiftL` 3)   .|. 
            index src
        weirdRSPHack src
        imm32 $ fromIntegral offset 
    | min8BitValI <= offset && offset <= max8BitValI = do
        rex_w (Just dst) (Just src)
        emit1 $ 0x8B
        -- operand 1 (dest)
        emit1 $ modRegRef8bitOffset .|. 
            (index dst `shiftL` 3)  .|. 
            index src
        weirdRSPHack src
        -- operand 2 (src)
        -- emit1 $ modReg .|. index src
        imm8 $ fromIntegral offset
-- Dereference (dst plus offset): refactor with the other mov?
mov_ o1@(RR64 dst offset) o2@(R64 src)
    | offset < min8BitValI || max8BitValI < offset = do 
        rex_w (Just dst) (Just src)
        emit1 $ 0x89
        emit1 $ modRegRef32bitOffset .|. (index src `shiftL` 3) .|. index dst
        weirdRSPHack dst
        imm32 $ fromIntegral offset
    | min8BitValI <= offset && offset <= max8BitValI = do
        rex_w (Just dst) (Just src)
        emit1 $ 0x89
        emit1 $ modRegRef8bitOffset .|. (index src `shiftL` 3) .|. index dst
        weirdRSPHack dst
        imm8  $ fromIntegral offset
mov_ o1@(RR64 dst offset) o2@(I64 src)
    | offset < min8BitValI || max8BitValI < offset = do 
        rex_w Nothing (Just dst)
        emit1 $ 0xC7
        emit1 $ modRegRef32bitOffset .|. index dst
        weirdRSPHack dst
        imm32 $ fromIntegral offset
        imm32 $ fromIntegral src
    | min8BitValI <= offset && offset <= max8BitValI = do
        rex_w Nothing (Just dst)
        emit1 $ 0xC7
        emit1 $ modRegRef8bitOffset .|. index dst
        weirdRSPHack dst
        imm8  $ fromIntegral offset
        imm32 $ fromIntegral src
mov_ o1@(RR64 dst offset) o2@(I8 src)
    | offset < min8BitValI || max8BitValI < offset = do 
        -- write single byte at address
        emit1 $ 0xC6
        emit1 $ modRegRef32bitOffset .|. index dst -- ModRM byte
        weirdRSPHack dst
        imm32 $ fromIntegral offset
        imm8  $ fromIntegral src
    | min8BitValI <= offset && offset <= max8BitValI = do
        -- write single byte at address
        emit1 $ 0xC6
        emit1 $ modRegRef8bitOffset .|. index dst -- ModRM byte
        weirdRSPHack dst
        imm8  $ fromIntegral offset
        imm8  $ fromIntegral src
mov_ o1@(RR64 dst offset) o2@(R8 src)
    | min8BitValI <= offset && offset <= max8BitValI = do
        emit1 $ 0x88
        emit1 $ modRegRef8bitOffset  .|.
            (index8 src `shiftL` 3)  .|.
            index dst
        imm8 $ fromIntegral offset
        -- undefined
mov_ o1@(R8 dst) (RR64 src offset) 
    | offset == 0 = do
        emit1 $ 0x8A
        emit1 $ (index8 dst `shiftL` 3) .|. index src
        weirdRSPHack src
    | min8BitValI <= offset && offset <= max8BitValI = do
        emit1 $ 0x8A
        emit1 $ modRegRef8bitOffset .|. (index8 dst `shiftL` 3) .|. index src
        imm8  $ fromIntegral offset
        weirdRSPHack src
mov_ o1 o2 = error $ "Unsupported mov operators: " ++ show o1 ++ " " ++ show o2

weirdRSPHack reg =
    if reg == RSP then 
        emit1 $ 0x24 -- FIXME: Find out why this is needed, unknown
    else 
        return ()

-- Interrupt execution (perform a system call)
int :: X86_64 ()
int = do
    docX86 "int"
    emit1 0xCD
    emit1 0x80
    asm bflush

add :: Val -> Val -> X86_64 ()
add a b = do
    docX86 $ "add " ++ show a ++ " " ++ show b
    binop baseByte 0 a b -- 0 means the "/0" in the table at p.623
    where baseByte (R64 RAX) (I32 _)    = 0x05
          baseByte (R64 _)   (I32 _)    = 0x81
          baseByte (R64 _)   (R64 _)    = 0x01
          baseByte (R64 _)   (RR64 _ _) = 0x03

sub :: Val -> Val -> X86_64 ()
sub a b = do
    docX86 $ "sub " ++ show a ++ " " ++ show b
    binop baseByte 5 a b -- Means the "/5" in the table at p.1836
    where baseByte (R64 RAX) (I32 _)    = 0x2D
          baseByte (R64 _)   (I32 _)    = 0x81
          baseByte (R64 _)   (R64 _)    = 0x29
          baseByte (R64 _)   (RR64 _ _) = 0x2B

and_ :: Val -> Val -> X86_64 ()
and_ a b = do
    docX86 $ "and " ++ show a ++ " " ++ show b
    binop baseByte 4 a b
    where baseByte (R64 RAX) (I32 _)    = 0x25
          baseByte (R64 _)   (I32 _)    = 0x81
          baseByte (R64 _)   (R64 _)    = 0x21
          baseByte (R64 _)   (RR64 _ _) = 0x23

cmp :: Val -> Val -> X86_64()
cmp a b = do
    docX86 $ "cmp " ++ show a ++ " " ++ show b
    binop baseByte 7 a b
    where baseByte (R64 RAX) (I32 _)    = 0x3D
          baseByte (R64 _)   (I32 _)    = 0x81
          baseByte (R64 _)   (R64 _)    = 0x39

binop ty oe o1@(R64 RAX) o2@(I32 val) = do
    rex_w Nothing Nothing
    emit1 $ ty o1 o2
    imm32 val
    asm $ bflush
binop ty oe o1@(R64 dst) o2@(I32 val) = do
    rex_w Nothing (Just dst)
    emit1 $ ty o1 o2
    emit1 $ modReg .|. opcodeExt oe .|. index dst
    imm32 val
    asm $ bflush
binop ty _ o1@(R64 dst) o2@(RR64 src offset) 
    | min8BitValI <= offset && offset <= max8BitValI = do
    rex_w (Just src) (Just dst)
    emit1 $ ty o1 o2
    emit1 $ modRegRef8bitOffset .|. index dst `shiftL` 3 .|. index src
    imm8  $ fromIntegral offset
    asm $ bflush
binop ty _ o1@(R64 dst) o2@(R64 src) = do
    rex_w (Just src) (Just dst)
    emit1 $ ty o1 o2
    emit1 $ mrmByte o2 o1
    asm $ bflush
binop _ _ o1 o2 = error $ "Unknown binops: " ++ show o1 ++ " " ++ show o2

-- Transfer the program control to a return address located on
-- the top of the stack. The address is usually placed on the stack
-- by a CALL instruction, and the return is made to the instruction
-- that follows the CALL instruction (Intel manual P.1725)
ret :: X86_64() 
ret = do
    docX86 "ret"
    emit1 0xc3
    asm $ bflush

-- Call the absolute address present in the given register.
call :: Val -> X86_64 ()
call r@(R64 src) = do
    -- Call operates on the call stack register rsp
    docX86 $ "call " ++ show r
    if isSupReg src then rex Nothing (Just src) else return ()
    emit1 0xFF
    emit1 $ mrmByte rdx r -- rdx is ignored but it's here for NASM compat.
    asm $ bflush

callLabel :: String -> X86_64 ()
callLabel label = do
    docX86 $ "call " ++ label
    emit1 0xE8
    asm $ emitLabelOff32 label
    asm $ bflush

je :: String -> X86_64()
-- Jump short if ZF=1. (i.e. If equal) 
-- Offset is a signed offset relative to the current program position.
je label = do
    docX86 $ "je " ++ label
    emit1 0x74
    asm $ emitLabelOff8 label
    asm $ bflush

-- je can only jump -/+128 so we introduce near jumps
jeNear label = do
    emit1 0x0F
    emit1 0x84
    asm $ emitLabelOff32 label
    asm $ bflush

jneNear label = do
    emit1 0x0F
    emit1 0x85
    asm $ emitLabelOff32 label
    asm $ bflush

jl :: String -> X86_64()
jl label = do
    emit1 0x7C
    asm $ emitLabelOff8 label
    asm $ bflush

jle :: String -> X86_64()
jle label = do
    emit1 0x7E
    asm $ emitLabelOff8 label
    asm $ bflush

jg :: String -> X86_64()
jg label = do
    emit1 0x7F
    asm $ emitLabelOff8 label
    asm $ bflush

jge :: String -> X86_64()
jge label = do
    emit1 0x7D
    asm $ emitLabelOff8 label
    asm $ bflush
  
jmp :: Val -> X86_64 ()
jmp (R64 reg) = do
    if isSupReg reg then rex Nothing (Just reg) else return ()
    emit1 0xFF
    emit1 $ 0xC0 .|. bit 5 .|. index reg -- ModRM byte
    asm $ bflush

jmpPtrOffset8 (R64 reg) offset = do
    if isSupReg reg then rex Nothing (Just reg) else return ()
    emit1 0xFF
    emit1 $ 0x40 .|. bit 5 .|. index reg  -- ModRM byte
    imm8 $ fromIntegral offset
    asm $ bflush

-- Logical compare, Intel manual P.1862
-- Computes the bit-wise logical AND between operands,
-- and sets the SF,ZF and PF status flags.
-- REX.W + 85 /r | TEST r/m64, r64
test :: Val -> Val -> X86_64()
test o1@(R64 r1) o2@(R64 r2) = do
    rex_w (Just r2) (Just r1)
    emit1 0x85
    emit1 $ mrmByte o2 o1
    asm $ bflush

inc :: Val -> X86_64 ()
inc r@(R64 dst) = do
    rex_w Nothing (Just dst)
    emit1 0xFF
    emit1 $ mrmByte rax r -- the 1nd operand is ignored
    asm $ bflush

-- Push word, doubleword or quadword onto the stack
push :: Val -> X86_64 ()
push (I64 val) = error $ "Unable to push a 64 bit literal! " ++
    "A workaround is to load it in a register, then push that register."
push (I32 val) = do
    emit1 0x68
    imm32 val -- Low-order byte first (checked)
    asm bflush
push (R64 src)
    | isSupReg src = do
    rex Nothing (Just src)
    emit1 $ 0x50 .|. index src
    asm bflush
push (R64 src) = do
    emit1 $ 0x50 .|. index src
    asm bflush
push (I8 val) = do
    emit1 0x6A -- Pushes an immediate 1 byte value and advances
    imm8  val  -- stack by 8 bytes! (Suprisingly.)
    asm bflush

jmpLabel :: String -> X86_64()
jmpLabel label = do
    docX86 $ "jmp"
    emit1 0xE9
    asm $ emitLabelOff32 label
    asm $ bflush

-- Multiply value in RAX by value from register passed
mul :: Val -> X86_64 ()
mul r@(R64 factor) = do
    rex_w Nothing (Just factor)
    emit1 0xF7
    emit1 $ modReg .|. opcodeExt 4 .|. index factor
    asm $ bflush

-- Shift left once, or shift left a number of times
-- Shift Arithmetic Left
sal :: Val -> Val -> X86_64 ()
sal (R64 reg) (I8 1) = do
    rex_w Nothing (Just reg)
    emit1 0xD1
    emit1 $ modReg .|. opcodeExt 4 .|. index reg
    asm $ bflush
sal (R64 reg) (I8 n) = do
    rex_w Nothing (Just reg)
    emit1 0xC1
    emit1 $ modReg .|. opcodeExt 4 .|. index reg
    imm8 n
    asm $ bflush
sal (R64 reg) (R8 CL) = do
    rex_w Nothing (Just reg)
    emit1 0xD3
    emit1 $ modReg .|. opcodeExt 4 .|. index reg
    asm $ bflush

-- Shift right
sar :: Val -> Val -> X86_64 ()
sar (R64 reg) (I8 1) = do
    rex_w Nothing (Just reg)
    emit1 0xD1
    emit1 $ modReg .|. opcodeExt 7 .|. index reg
    asm $ bflush
sar (R64 reg) (I8 n) = do
    rex_w Nothing (Just reg)
    emit1 0xC1
    emit1 $ modReg .|. opcodeExt 7 .|. index reg
    imm8 n
    asm $ bflush
sar (R64 reg) (R8 CL) = do
    rex_w Nothing (Just reg)
    emit1 0xD3
    emit1 $ modReg .|. opcodeExt 7 .|. index reg
    asm $ bflush

-- Intel manual P.2504, XOR
xor :: Val -> Val -> X86_64 ()
xor o1@(R64 dst) o2@(R64 src) = do -- REX.W + 31 /r | XOR r/m64, r64
    rex_w (Just src) (Just dst)
    emit1 0x31
    emit1 $ mrmByte o2 o1
    asm bflush

