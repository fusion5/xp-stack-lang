{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}

module X64.X64 where


import Data.Word
import Data.Bits
import Data.Binary.Put -- (putWord64le, putWord32le, Put, runPut)
import Data.Int
import Data.Char (ord)

import qualified Data.ByteString.Lazy as BS
-- import qualified Data.ByteString.Internal as BSI (c2w, w2c)
-- import Data.ByteString.Lazy.Char8 ({- ByteString, -} unpack, split)
import qualified Data.Sequence as S
import qualified Data.Map as M

import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Except

import ASM.ASM
import ASM.Datatypes
import X64.Datatypes

-- Initial assembler state for the x86-64.
istateX64 :: Word64 -> ASMState Word64
istateX64 image_base_address = ASMState
    { file_addr  = 0
    , rva        = 0 -- Often refered to as 'RVA'
    , image_base = image_base_address
    , contents   = S.empty
    , labels     = M.empty
    , uid        = 0
    , zero       = 0
    , resolved_asm = BS.empty -- The assembly consisting only of bytes (label references etc. resolved)
    }

runASM_for_X64 :: Word64 -> ASM Word64 a -> Either String (ASMState Word64)
runASM_for_X64 image_base_address = runExcept . flip execStateT (istateX64 image_base_address)

deref :: Operand -> Operand
deref o = derefOffset o 0

derefOffset :: Operand -> Integer -> Operand
derefOffset (R64 r) offset = RR64 r offset
derefOffset _ _ = error 
    "References with Offsets can only be applied to registers"

currFileAddr :: X64 Word64
currFileAddr = runASM $ do
    s <- get
    return $ file_addr s

currRVA :: X64 Word64
currRVA = runASM $ do
    s <- get
    return $ rva s

emitOpcode :: String -> Opcode () -> X64 ()
emitOpcode nasmEquiv op = do
    opcodeBytes <- execWriterT op
    runASM $ do
        state <- get
        modify $ ASM.ASM.append $ 
            ASMOpcode (file_addr state) 
                      (rva state) 
                      opcodeBytes
                      nasmEquiv

ascii :: Char -> Word8
ascii = fromIntegral . ord

emitBytes :: ASMBytes Word64 -> X64 ()
emitBytes bytes = runASM $ do
    state <- get
    modify $ ASM.ASM.append $ 
        ASMBytes (file_addr state) 
                 (rva state) 
                 (S.singleton bytes)


emitString :: String -> X64 ()
emitString = runASM . stringASM

labelDiff32 :: String -> String -> ASMBytes Word64
labelDiff32 = BytesLabelDiff RVA Bytes4 LE

imageLabelDiff32 :: String -> String -> ASMBytes Word64
imageLabelDiff32 = BytesLabelDiff IA Bytes4 LE

labelDiff16 :: String -> String -> ASMBytes Word64
labelDiff16 = BytesLabelDiff RVA Bytes2 LE

int64 :: Word64 -> ASMBytes Word64
int64 = BytesLiteral . runPut . putWord64le 

int32 :: Word32 -> ASMBytes Word64
int32 = BytesLiteral . runPut . putWord32le 

int8 :: Word8 -> ASMBytes Word64
int8 = BytesLiteral . BS.singleton

-- Opcode monad helpers that help us to generate label references 
-- for generating CPU opcodes
opcodeLabelRef64 :: String -> Opcode ()
opcodeLabelRef64 = tell . S.singleton . BytesLabelRef VA Bytes8 LE 

opcodeLabelOff32 :: Word64 -> Word64 -> String -> Opcode ()
opcodeLabelOff32 file_addr rva = 
    tell . S.singleton . BytesLabelRefOffset file_addr rva VA Bytes4 LE 

opcodeLabelOff8 :: Word64 -> Word64 -> String -> Opcode ()
opcodeLabelOff8 file_addr rva = 
    tell . S.singleton . BytesLabelRefOffset file_addr rva VA Bytes1 LE 

opcodeLabelDiff32 :: String -> String -> Opcode ()
opcodeLabelDiff32 l1 l2 = tell (S.singleton (BytesLabelDiff RVA Bytes4 LE l1 l2))

opcodeLabelDiff16 :: String -> String -> Opcode ()
opcodeLabelDiff16 l1 l2 = tell (S.singleton (BytesLabelDiff RVA Bytes2 LE l1 l2))

emit1 :: Word8 -> Opcode ()
emit1 = tell . S.singleton . int8

imm64 :: Word64 -> Opcode () -- X86 uses little endian encoding for integers
imm64 = tell . S.singleton . int64

imm32 :: Word32 -> Opcode () -- X86 uses little endian encoding for integers
imm32 = tell . S.singleton . int32

imm8  :: Word8 -> Opcode () -- X86 uses little endian encoding for integers
imm8  = emit1

max64BitUnsigned = fromIntegral (maxBound::Word64)
max32BitUnsigned = fromIntegral (maxBound::Word32)

max8BitValI :: Integer
max8BitValI = fromIntegral (maxBound::Int8)

min8BitValI :: Integer
min8BitValI = fromIntegral (minBound::Int8)

{- BS.pack $ Prelude.map BSI.c2w bs
  where
    bs = unpack $ runPut $ putFun (fromIntegral x) -}

-- The REX value is in binary "0100WRXB" where W,R,X and B are bits.
-- Detailed here: https://wiki.osdev.org/index.php?title=X86-64_Instruction_Encoding
word_rex, word_rex_w, word_rex_b, word_rex_r, word_rex_x :: Word8
word_rex   = bit 6
word_rex_w = bit 3 -- Indicates that the instruction is promoted to 64 bits
word_rex_r = bit 2 -- Permits access to registers R8-R15 of an operand
word_rex_b = bit 0 -- Permits access to registers R8-R15 of an operand
word_rex_x = bit 1 -- Unknown what this does

-- REX is a prefix often used in 64 bit mode instructions.
rex   a b = emit1 $ rex_   a b
rex_w a b = emit1 $ rex_w_ a b

rex_b reg = if isSupReg reg then emit1 (word_rex .|. word_rex_b) else return ()

rex_w_ a b = word_rex_w .|. rex_ a b
rex_ mayOper1 mayOper2 = word_rex 
    .|. b mayOper1 word_rex_r
    .|. b mayOper2 word_rex_b
    where b Nothing      _ = 0
          b (Just reg) w = if isSupReg reg then w else 0

{- These modes are the first 2 bits of the ModRM Byte
 - See Intel Manual P.528, ModR/M byte definition
 - ModRM byte breakdown:
 -   7   6 | 5   4   3 | 2   1   0 
 - [ MOD   | Reg/Opcode| R/M       ]
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
mrmByte :: Operand -- Register 
        -> Operand -- Register or Memory
        -> Word8
mrmByte (R64 reg) (R64 rM) = 
    modReg .|. (index reg `shiftL` 3) .|. index rM

-- The bits that form a specific ModR/M byte opcode extension
-- bits 3, 4, 5, usually found in the Intel manual as /0, /1,
-- etc., up to /7
opcodeExt :: Integer -> Word8
opcodeExt x | 0 <= x && x <= 7 = 
    fromIntegral x `shiftL` 3

pop :: Operand -> X64 ()
pop (R64 to) = emitOpcode ("pop " ++ show to) $ do
    if isSupReg to then rex Nothing (Just to) else return ()
    emit1 $ 0x58 .|. index to

xor :: Operand -> Operand -> X64 ()
xor o1@(R64 to) o2@(R64 from) = emitOpcode ("xor " ++ show to ++ ", " ++ show from) $ do
    -- REX.W + 31 /r | XOR r/m64, r64
    rex_w (Just from) (Just to)
    emit1 0x31
    emit1 $ mrmByte o2 o1
    
mov :: Operand -> Operand -> X64 ()
mov to from = emitOpcode ("mov " ++ show to ++ ", " ++ show from) $ do
    mov_ to from
  where
    -- Intel manual P.1207/5038
    mov_ :: Operand -> Operand -> Opcode ()
    -- Emit immediate data to register
    mov_ (R64 dst) (I64 src) = do       
        rex_w Nothing (Just dst)   -- REX.W + B8 +rd io | MOV r64, imm64
        emit1 $ 0xB8 .|. index dst -- Which register in the last 3 bits of 0xB8
        imm64 src
    mov_ (R64 dst) (L64 label) = do
        rex_w Nothing (Just dst) 
        emit1 $ 0xB8 .|. index dst
        opcodeLabelRef64 label
    {- String literal support disabled, will be achieved by means of labels.
    mov_ (R64 dst) (S64 str) = do
        rex_w Nothing (Just dst) 
        emit1 $ 0xB8 .|. index dst
        asm $ emitStringRef str
    -}
    mov_ o1@(R64 dst) o2@(R64 src) = do -- REX.W + 89 /r | MOV r/m64, r64
        rex_w (Just src) (Just dst)
        emit1 $ 0x89
        emit1 $ mrmByte o2 o1
    mov_ o1@(R64 dst) o2@(RR64 src offset)
        | offset == 0 && (src /= RBP) = do 
            rex_w (Just dst) (Just src)
            emit1 $ 0x8B
            emit1 $ modRegRef .|. 
                (index dst `shiftL` 3)   .|.
                index src
            weirdRSPHack src
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
            imm8 $ fromIntegral offset
    -- Dereference (dst plus offset): refactor with the other mov?
    mov_ o1@(RR64 dst offset) o2@(R64 src)
        | offset == 0 = do
            rex_w (Just src) (Just dst)
            emit1 $ 0x89
            emit1 $ modRegRef .|. (index src `shiftL` 3) .|. index dst
            weirdRSPHack dst
        | offset < min8BitValI || max8BitValI < offset = do
            rex_w (Just src) (Just dst)
            emit1 $ 0x89
            emit1 $ modRegRef32bitOffset .|. (index src `shiftL` 3) .|. index dst
            weirdRSPHack dst
            imm32 $ fromIntegral offset
        | min8BitValI <= offset && offset <= max8BitValI = do
            rex_w (Just src) (Just dst)
            emit1 $ 0x89
            emit1 $ modRegRef8bitOffset .|. (index src `shiftL` 3) .|. index dst
            weirdRSPHack dst
            imm8  $ fromIntegral offset
    mov_ o1@(RR64 dst offset) o2@(I32 src)
        -- WARNING! Performs sign-extension of I32 to 64 bits!
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
            rex_b dst
            emit1 $ 0xC6
            emit1 $ modRegRef32bitOffset .|. index dst -- ModRM byte
            weirdRSPHack dst
            imm32 $ fromIntegral offset
            imm8  $ fromIntegral src
        | min8BitValI <= offset && offset <= max8BitValI = do
            -- write single byte at address
            rex_b dst
            emit1 $ 0xC6
            emit1 $ modRegRef8bitOffset .|. index dst -- ModRM byte
            weirdRSPHack dst
            imm8  $ fromIntegral offset
            imm8  $ fromIntegral src
    mov_ o1@(RR64 dst offset) o2@(R8 src)
        | min8BitValI <= offset && offset <= max8BitValI = do
            rex_b dst
            emit1 $ 0x88
            emit1 $ modRegRef8bitOffset  .|.
                (index8 src `shiftL` 3)  .|.
                index dst
            weirdRSPHack dst
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

-- Interrupt execution (perform a system call on Linux)
int :: X64 ()
int = emitOpcode "int" $ do
    emit1 0xCD
    emit1 0x80

int3 :: X64 ()
int3 = emitOpcode "int3" $ do
    emit1 0xCC

binop ty oe o1@(R64 RAX) o2@(I32 val) = do
    rex_w Nothing Nothing
    emit1 $ ty o1 o2
    imm32 val
binop ty oe o1@(R64 dst) o2@(I32 val) = do
    rex_w Nothing (Just dst)
    emit1 $ ty o1 o2
    emit1 $ modReg .|. opcodeExt oe .|. index dst
    imm32 val
binop ty _ o1@(R64 dst) o2@(RR64 src offset) 
    | min8BitValI <= offset && offset <= max8BitValI = do
    rex_w (Just dst) (Just src)
    emit1 $ ty o1 o2
    emit1 $ modRegRef8bitOffset .|. index dst `shiftL` 3 .|. index src
    imm8  $ fromIntegral offset
binop ty _ o1@(R64 dst) o2@(R64 src) = do
    rex_w (Just src) (Just dst)
    emit1 $ ty o1 o2
    emit1 $ mrmByte o2 o1
binop _ _ o1 o2 = error $ "Unsupported operands: " ++ show o1 ++ " " ++ show o2

add :: Operand -> Operand -> X64 ()
add a b = emitOpcode ("add " ++ show a ++ ", " ++ show b) $ do
    binop baseByte 0 a b -- 0 means the "/0" in the table at p.623
    where baseByte (R64 RAX) (I32 _)    = 0x05
          baseByte (R64 _)   (I32 _)    = 0x81
          baseByte (R64 _)   (R64 _)    = 0x01
          baseByte (R64 _)   (RR64 _ _) = 0x03

sub :: Operand -> Operand -> X64 ()
sub a b = emitOpcode ("sub " ++ show a ++ " " ++ show b) $ do
    binop baseByte 5 a b -- Means the "/5" in the table at p.1836
    where baseByte (R64 RAX) (I32 _)    = 0x2D
          baseByte (R64 _)   (I32 _)    = 0x81
          baseByte (R64 _)   (R64 _)    = 0x29
          baseByte (R64 _)   (RR64 _ _) = 0x2B

and_ :: Operand -> Operand -> X64 ()
and_ a b = emitOpcode ("and " ++ show a ++ " " ++ show b) $ do
    binop baseByte 4 a b
    where baseByte (R64 RAX) (I32 _)    = 0x25
          baseByte (R64 _)   (I32 _)    = 0x81
          baseByte (R64 _)   (R64 _)    = 0x21
          baseByte (R64 _)   (RR64 _ _) = 0x23

cmp :: Operand -> Operand -> X64 ()
cmp a b = emitOpcode ("cmp " ++ show a ++ " " ++ show b) $ do
    binop baseByte 7 a b
    where baseByte (R64 RAX) (I32 _)    = 0x3D
          baseByte (R64 _)   (I32 _)    = 0x81
          baseByte (R64 _)   (R64 _)    = 0x39

-- Transfer the program control to a return address located on
-- the top of the stack. The address is usually placed on the stack
-- by a CALL instruction, and the return is made to the instruction
-- that follows the CALL instruction (Intel manual P.1725)
ret :: X64 () 
ret = emitOpcode "ret" $ emit1 0xC3

-- Call the absolute address present in the given register.
-- Operates on the call stack register rsp
call :: Operand -> X64 ()
call r@(R64 src) = emitOpcode ("call " ++ show r) $ do
    if isSupReg src then rex Nothing (Just src) else return ()
    emit1 0xFF
    emit1 $ mrmByte rdx r -- rdx is ignored but it's here for NASM compat.
call r@(L64 label) = do
    file_addr <- currFileAddr
    rva       <- currRVA
    emitOpcode ("call " ++ label) $ do
        emit1 0xE8
        -- The +1 is needed because the offset is relative to
        -- the end of the opcode. We have emitted 1 byte so far,
        -- therefore we need to shift the addresses by 1
        opcodeLabelOff32 (file_addr+1) (rva+1) label

callLabel :: String -> X64 ()
callLabel l = do
    call $ L64 l

-- je can only jump -/+128; for longer jumps there are near jumps
-- The offset is from the beginning of the command:
-- "a signed offset relative to the current value of the instruc-
-- tion pointer in the EIP register"
jNear code nasm_instr label = do
    file_addr <- currFileAddr
    rva       <- currRVA
    emitOpcode (nasm_instr ++ " " ++ label) $ do
        emit1 0x0F
        emit1 code
        -- Why +2: see the call function
        opcodeLabelOff32 (file_addr+2) (rva+2) label

joNear  = jNear 0x80 "jo"  -- Jump on overflow
jnoNear = jNear 0x81 "jno" -- Jump on not overflow
jcNear  = jNear 0x82 "jc"  -- Jump on CF=1
jncNear = jNear 0x83 "jnc" -- Jump on CF=0
jeNear  = jNear 0x84 "je"  
jneNear = jNear 0x85 "jne" 
jlNear  = jNear 0x8C "jl"  
jgeNear = jNear 0x8D "jge" 
jleNear = jNear 0x8E "jle" 
jgNear  = jNear 0x8F "jg"  

j code nasm_instr label = do
    file_addr <- currFileAddr
    rva       <- currRVA
    emitOpcode (nasm_instr ++ " " ++ label) $ do
       emit1 code 
       opcodeLabelOff8 (file_addr+1) (rva+1) label

jo  = j 0x70 "jo"
jno = j 0x71 "jno"
jc  = j 0x72 "jc"
jnc = j 0x73 "jnc"
je  = j 0x74 "je"
jne = j 0x75 "jne"
jl  = j 0x7C "jl"
jge = j 0x7D "jge"
jle = j 0x7E "jle"
jg  = j 0x7F "jg"

jmp :: Operand -> X64 ()
jmp r@(R64 reg) = emitOpcode ("jmp " ++ show r) $ do
    if isSupReg reg then rex Nothing (Just reg) else return ()
    emit1 0xFF
    emit1 $ 0xC0 .|. bit 5 .|. index reg -- ModRM byte
jmp r@(L64 label) = do
    file_addr <- currFileAddr
    rva       <- currRVA
    emitOpcode ("jmp " ++ label) $ do
        emit1 0xE9
        -- Why +1: see the call function
        opcodeLabelOff32 (file_addr+1) (rva+1) label

jmpLabel :: String -> X64 ()
jmpLabel = jmp . L64    

jmpPtrOffset8 r@(R64 reg) offset = 
    emitOpcode ("jmp " ++ show r ++ " " ++ show offset) $ do
        if isSupReg reg then rex Nothing (Just reg) else return ()
        emit1 0xFF
        emit1 $ 0x40 .|. bit 5 .|. index reg  -- ModRM byte
        imm8  offset

-- Logical compare, Intel manual P.1862
-- Computes the bit-wise logical AND between operands,
-- and sets the SF,ZF and PF status flags.
-- REX.W + 85 /r | TEST r/m64, r64
test :: Operand -> Operand -> X64 ()
test o1@(R64 r1) o2@(R64 r2) = 
    emitOpcode ("test " ++ show o1 ++ " " ++ show o2) $ do
        rex_w (Just r2) (Just r1)
        emit1 0x85
        emit1 $ mrmByte o2 o1

setz :: Operand -> X64 ()
setz o@(R8 r) = 
    emitOpcode ("setz " ++ show r) $ do
        emit1 0x0F 
        emit1 0x94
        -- MRM byte
        emit1 $ modReg .|. index8 r

inc :: Operand -> X64 ()
inc r@(R64 dst) = emitOpcode ("inc " ++ show r) $ do
    rex_w Nothing (Just dst)
    emit1 0xFF
    emit1 $ mrmByte rax r -- the 1nd operand is ignored

dec :: Operand -> X64 ()
dec r@(R64 dst) = emitOpcode ("dec " ++ show r) $ do
    rex_w Nothing (Just dst)
    emit1 0xFF
    emit1 $ mrmByte rax r .|. opcodeExt 1

-- Push word, doubleword or quadword onto the stack

push :: Operand -> X64 () 
push op = emitOpcode ("push " ++ show op) $ push_ op

push_ :: Operand -> Opcode ()
push_ (I64 val) = error $ "Unable to push a 64 bit literal! " ++
    "A workaround is to load it in a register, then push that register."
push_ (I32 val) = do
    emit1 0x68
    imm32 val -- Low-order byte first (checked)
push_ (R64 src)
    | isSupReg src = do
    rex Nothing (Just src)
    emit1 $ 0x50 .|. index src
push_ (R64 src) = do
    emit1 $ 0x50 .|. index src
push_ (I8 val) = do
    emit1 0x6A -- Pushes an immediate 1 byte value and advances
    imm8  val  -- stack by 8 bytes! (Suprisingly.)

-- Multiply value in RAX by value from register passed; stores
-- the result in RDX:RAX!
mul :: Operand -> X64 ()
mul r@(R64 factor) = emitOpcode ("mul " ++ show r) $ do
    rex_w Nothing (Just factor)
    emit1 0xF7
    emit1 $ modReg .|. opcodeExt 4 .|. index factor

-- It's named div_ to avoid the conflict with the Prelude div
div_ :: Operand -> X64 ()
div_ r@(R64 factor) = emitOpcode ("div " ++ show r) $ do
    rex_w Nothing (Just factor)
    emit1 0xF7
    emit1 $ modReg .|. opcodeExt 6 .|. index factor

-- Shift left once, or shift left a number of times
-- Shift Arithmetic Left
sal :: Operand -> Operand -> X64 ()
sal op1 op2 = emitOpcode ("sal " ++ show op1 ++ " " ++ show op2) (sal_ op1 op2)
  where 
    sal_ :: Operand -> Operand -> Opcode ()
    sal_ (R64 reg) (I8 1) = do
        rex_w Nothing (Just reg)
        emit1 0xD1
        emit1 $ modReg .|. opcodeExt 4 .|. index reg
    sal_ (R64 reg) (I8 n) = do
        rex_w Nothing (Just reg)
        emit1 0xC1
        emit1 $ modReg .|. opcodeExt 4 .|. index reg
        imm8 n
    sal_ (R64 reg) (R8 CL) = do
        rex_w Nothing (Just reg)
        emit1 0xD3
        emit1 $ modReg .|. opcodeExt 4 .|. index reg

-- Shift right
sar :: Operand -> Operand -> X64 () 
sar op1 op2 = emitOpcode ("sar " ++ show op1 ++ " " ++ show op2) (sar_ op1 op2)
  where
    sar_ :: Operand -> Operand -> Opcode ()
    sar_ (R64 reg) (I8 1) = do
        rex_w Nothing (Just reg)
        emit1 0xD1
        emit1 $ modReg .|. opcodeExt 7 .|. index reg
    sar_ (R64 reg) (I8 n) = do
        rex_w Nothing (Just reg)
        emit1 0xC1
        emit1 $ modReg .|. opcodeExt 7 .|. index reg
        imm8 n
    sar_ (R64 reg) (R8 CL) = do
        rex_w Nothing (Just reg)
        emit1 0xD3
        emit1 $ modReg .|. opcodeExt 7 .|. index reg

