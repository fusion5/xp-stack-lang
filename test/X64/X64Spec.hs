{-# Language FlexibleContexts #-}

module X64.X64Spec (spec, encode) where

import           Test.Hspec
import           X64.Types
import           X64.X64
import qualified ASM.ASM              as ASM
import qualified ASM.Types            as ASM
import qualified Data.ByteString.Lazy as BS
import qualified Data.Either          as Either
import qualified Data.Word            as Word
import qualified X64.Types            as X64
import qualified X64.X64              as X64


spec :: Spec
spec = do
  spec_00050
  spec_00100


encode :: X64.X64 () -> [Word.Word8]
encode act =
  case X64.runASM_for_X64 0x400000 (X64.runX64 act >> ASM.compileASM) of
    Left err -> error err
    Right st -> BS.unpack $ ASM.resolved_asm st


spec_00050 :: Spec
spec_00050 = do
  it ".TEST" $
    encode (ASM.label "TEST")
      `shouldBe` []


spec_00100 :: Spec
spec_00100 = do
  it "mov rax, 0x7766554433221100" $
    encode (mov rax (I64 0x7766554433221100))
      `shouldBe` [0x48,0xB8,0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77]
  it "mov r11, 0x7766554433221100" $
    encode (mov r11 (I64 0x7766554433221100))
      `shouldBe` [0x49,0xBB,0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77]
  it "mov rbx, 0x2A" $
    encode (mov rbx (I64 0x2A))
      `shouldBe` [0x48,0xBB,0x2A,0x00,0x00,0x00,0x00,0x00,0x00,0x00]
  it "mov rsp, rcx" $
    encode (mov rsp rcx)
      `shouldBe` [0x48,0x89,0xCC]
  it "mov rdx, [rbp]" $
    encode (mov rdx (rbp `derefOffset` 0))
      `shouldBe` [0x48,0x8B,0x55,0x00]
  it "mov rax, [rbp]" $
    encode (mov rax (rbp `derefOffset` 0))
      `shouldBe` [0x48,0x8B,0x45,0x00]
  it "mov rdx, [rdi - 32768]" $
    encode (mov rdx (rdi `derefOffset` (-32768)))
      `shouldBe` [0x48,0x8B,0x97,0x00,0x80,0xFF,0xFF]
  it "mov rdx, [rdi - 32767]" $
    encode (mov rdx (rdi `derefOffset` (-32767)))
      `shouldBe` [0x48,0x8B,0x97,0x01,0x80,0xFF,0xFF]
  it "mov rdx, [rdi - 129]" $
    encode (mov rdx (rdi `derefOffset` (-129)))
      `shouldBe` [0x48,0x8B,0x97,0x7F,0xFF,0xFF,0xFF]
  it "mov rdx, [rdi - 128]" $
    encode (mov rdx (rdi `derefOffset` (-128)))
      `shouldBe` [0x48,0x8B,0x57,0x80]
  it "mov rdx, [rdi - 127]" $
    encode (mov rdx (rdi `derefOffset` (-127)))
      `shouldBe` [0x48,0x8B,0x57,0x81]
  it "mov rdx, [rdi + 127]" $
    encode (mov rdx (rdi `derefOffset` 127))
      `shouldBe` [0x48,0x8B,0x57,0x7F]
  it "mov rdx, [rdi + 128]" $
    encode (mov rdx (rdi `derefOffset` 128))
      `shouldBe` [0x48,0x8B,0x97,0x80,0x00,0x00,0x00]
  it "mov rdx, [rdi + 129]" $
    encode (mov rdx (rdi `derefOffset` 129))
      `shouldBe` [0x48,0x8B,0x97,0x81,0x00,0x00,0x00]
  it "mov rdx, [rdi + 32768]" $
    encode (mov rdx (rdi `derefOffset` (32768)))
      `shouldBe` [0x48,0x8B,0x97,0x00,0x80,0x00,0x00]
  it "mov qword [rbp + 127], 32" $
    encode (mov (rbp `derefOffset` 127) (I32 32))
      `shouldBe` [0x48,0xC7,0x45,0x7F,0x20,0x00,0x00,0x00]
  it "mov qword [rbp + 128], 32" $
    encode (mov (rbp `derefOffset` 128) (I32 32))
      `shouldBe` [0x48,0xC7,0x85,0x80,0x00,0x00,0x00,0x20,0x00,0x00,0x00]
  it "mov byte  [rbp + 127], 0x48" $
    encode (mov (rbp `derefOffset` 127) (I8  0x48))
      `shouldBe` [0xC6,0x45,0x7F,0x48]
  it "mov byte  [rbp],   0xEA" $
    encode (mov (rbp `derefOffset` 0)   (I8  0xEA))
      `shouldBe` [0xC6,0x45,0x00,0xEA]
  it "mov byte  bl, [rdi+1]" $
    encode (mov (RL8 BX) (rdi `derefOffset` 1))
      `shouldBe` [0x8A,0x5F,0x01]
  it "mov byte  al, [rdi]" $
    encode (mov (RL8 AX) (rdi `derefOffset` 0))
      `shouldBe` [0x8A,0x07]
  it "mov byte  cl, [rcx]" $
    encode (mov (RL8 CX) (rcx `derefOffset` 0))
      `shouldBe` [0x8A,0x09]
  it "mov byte  [rbp + 64], cl" $
    encode (mov (rbp `derefOffset` 64) cl)
      `shouldBe` [0x88,0x4D,0x40]
  it "mov qword rsi, [rsi + 8]" $
    encode (mov rsi (rsi `derefOffset` 8))
      `shouldBe` [0x48,0x8B,0x76,0x08]
  it "mov qword rax, [rsp + 8]" $
    encode (mov rax (rsp `derefOffset` 8))
      `shouldBe` [0x48,0x8B,0x44,0x24,0x08]
  it "mov qword [rsp + 8],   rax" $
    encode (mov (rsp `derefOffset` 8) rax)
      `shouldBe` [0x48,0x89,0x44,0x24,0x08]
  it "mov qword [rsp + 255], rbx" $
    encode (mov (rsp `derefOffset` 255) rbx)
      `shouldBe` [0x48,0x89,0x9C,0x24,0xFF,0x00,0x00,0x00]
  it "mov qword [rsp + 8],   100" $
    encode (mov (rsp `derefOffset` 8) (I32 100))
      `shouldBe` [0x48,0xC7,0x44,0x24,0x08,0x64,0x00,0x00,0x00]
  it "mov qword [rsp + 255], 100" $
    encode (mov (rsp `derefOffset` 255) (I32 100))
      `shouldBe` [0x48,0xC7,0x84,0x24,0xFF,0x00,0x00,0x00,0x64,0x00,0x00,0x00]
  it "mov byte  [rsp + 255], 0xEA" $
    encode (mov (rsp `derefOffset` 255) (I8 0xEA))
      `shouldBe` [0xC6,0x84,0x24,0xFF,0x00,0x00,0x00,0xEA]
  it "mov byte  [rsp + 8],   0xEA" $
    encode (mov (rsp `derefOffset` 8)   (I8 0xEA))
      `shouldBe` [0xC6,0x44,0x24,0x08,0xEA]
  it "mov byte  [r9  + 8],   0xEA" $
    encode (mov (r9  `derefOffset` 8)   (I8 0xEA))
      `shouldBe` [0x41,0xC6,0x41,0x08,0xEA]
  it "mov byte  [r9  + 8],   cl" $
    encode (mov (r9  `derefOffset` 8)   cl)
      `shouldBe` [0x41,0x88,0x49,0x08]
  it "mov byte al, [rsp+0]" $
    encode (mov (RL8 AX) (rsp `derefOffset` 0))
      `shouldBe` [0x8A,0x04,0x24]
  it "mov byte al, [rcx+0]" $
    encode (mov (RL8 AX) (rcx `derefOffset` 0))
      `shouldBe` [0x8A,0x01]
  it "mov rax,     [r9+1]" $
    encode (mov rax (derefOffset r9 1))
      `shouldBe` [0x49,0x8B,0x41,0x01]
  it "mov rax,     [rax+1]" $
    encode (mov rax (derefOffset rax 1))
      `shouldBe` [0x48,0x8B,0x40,0x01]
  it "mov rax,     [rsi]" $
    encode (mov rax (derefOffset rsi 0))
      `shouldBe` [0x48,0x8B,0x06]
  it "mov r9,      [rax+1]" $
    encode (mov r9 (derefOffset rax 1))
      `shouldBe` [0x4C,0x8B,0x48,0x01]
  it "mov r9,      [r8+1]" $
    encode (mov r9 (derefOffset r8 1))
      `shouldBe` [0x4D,0x8B,0x48,0x01]
  it "mov r9,      [rax]" $
    encode (mov r9 (derefOffset rax 0))
      `shouldBe` [0x4C,0x8B,0x08]
  it "mov r9,      [r8]" $
    encode (mov r9 (derefOffset r8 0))
      `shouldBe` [0x4D,0x8B,0x08]
  it "mov rsp,     [r8]" $
    encode (mov rsp (derefOffset r8 0))
      `shouldBe` [0x49,0x8B,0x20]
  it "mov r9,      [rsp]" $
    encode (mov r9 (derefOffset rsp 0))
      `shouldBe` [0x4C,0x8B,0x0C,0x24]
  it "mov qword [rsi], r15" $
    encode (mov (derefOffset rsi 0) r15)
      `shouldBe` [0x4C,0x89,0x3E]
  it "mov qword [rsp], r15" $
    encode (mov (derefOffset rsp 0) r15)
      `shouldBe` [0x4C,0x89,0x3C,0x24]
  it "mov qword [r11], rax" $
    encode (mov (derefOffset r11 0) rax)
      `shouldBe` [0x49,0x89,0x03]
  it "mov qword [r11], rsp" $
    encode (mov (derefOffset r11 0) rsp)
      `shouldBe` [0x49,0x89,0x23]
  it "mov qword [rsi + 1], r15" $
    encode (mov (derefOffset rsi 1) r15)
      `shouldBe` [0x4C,0x89,0x7E,0x01]
  it "mov qword [rsp + 1], r15" $
    encode (mov (derefOffset rsp 1) r15)
      `shouldBe` [0x4C,0x89,0x7C,0x24,0x01]
  it "mov qword [r11 + 1], rax" $
    encode (mov (derefOffset r11 1) rax)
      `shouldBe` [0x49,0x89,0x43,0x01]
  it "mov qword [r11 + 1], rsp" $
    encode (mov (derefOffset r11 1) rsp)
      `shouldBe` [0x49,0x89,0x63,0x01]
  it "mov qword [rsi + 1], rax" $
    encode (mov (derefOffset rsi 1) rax)
      `shouldBe` [0x48,0x89,0x46,0x01]
  it "mov qword [rsi + 1024], r15" $
    encode (mov (derefOffset rsi 1024) r15)
      `shouldBe` [0x4C,0x89,0xBE,0x00,0x04,0x00,0x00]
  it "mov qword [rsp + 1024], r15" $
    encode (mov (derefOffset rsp 1024) r15)
      `shouldBe` [0x4C,0x89,0xBC,0x24,0x00,0x04,0x00,0x00]
  it "mov qword [r11 + 1024], rax" $
    encode (mov (derefOffset r11 1024) rax)
      `shouldBe` [0x49,0x89,0x83,0x00,0x04,0x00,0x00]
  it "mov qword [r11 + 1024], rsp" $
    encode (mov (derefOffset r11 1024) rsp)
      `shouldBe` [0x49,0x89,0xA3,0x00,0x04,0x00,0x00]
  it "mov qword [r11 + 1024], 0x100" $
    encode (mov (derefOffset r11 1024) (I32 0x100))
      `shouldBe` [0x49,0xC7,0x83,0x00,0x04,0x00,0x00,0x00,0x01,0x00,0x00]
  it "mov qword [r11 + 64],   0x100" $
    encode (mov (derefOffset r11 64)   (I32 0x100))
      `shouldBe` [0x49,0xC7,0x43,0x40,0x00,0x01,0x00,0x00]
  it "mov qword [r12], rax" $
    encode (mov (derefOffset r12 0) rax)
      `shouldBe` [0x49,0x89,0x04,0x24]
  it "mov qword [r13], rax" $
    encode (mov (derefOffset r13 0) rax)
      `shouldBe` [0x49,0x89,0x45,0x00]
  it "mov qword [rbp], rax" $
    encode (mov (derefOffset rbp 0) rax)
      `shouldBe` [0x48,0x89,0x45,0x00]
  it "mov qword [rsp], rax" $
    encode (mov (derefOffset rsp 0) rax)
      `shouldBe` [0x48,0x89,0x04,0x24]
  it "mov qword [r12], rbp" $
    encode (mov (derefOffset r12 0) rbp)
      `shouldBe` [0x49,0x89,0x2C,0x24]
  it "mov qword [r13], rsp" $
    encode (mov (derefOffset r13 0) rsp)
      `shouldBe` [0x49,0x89,0x65,0x00]
  it "mov qword [rbp], r13" $
    encode (mov (derefOffset rbp 0) r13)
      `shouldBe` [0x4C,0x89,0x6D,0x00]
  it "mov qword [rsp], r12" $
    encode (mov (derefOffset rsp 0) r12)
      `shouldBe` [0x4C,0x89,0x24,0x24]
  it "mov qword [r12+1], rbp" $
    encode (mov (derefOffset r12 1) rbp)
      `shouldBe` [0x49,0x89,0x6C,0x24,0x01]
  it "mov qword [r13+1], rsp" $
    encode (mov (derefOffset r13 1) rsp)
      `shouldBe` [0x49,0x89,0x65,0x01]
  it "mov qword [rbp+1], r13" $
    encode (mov (derefOffset rbp 1) r13)
      `shouldBe` [0x4C,0x89,0x6D,0x01]
  it "mov qword [rsp+1], r12" $
    encode (mov (derefOffset rsp 1) r12)
      `shouldBe` [0x4C,0x89,0x64,0x24,0x01]
  it "mov qword [r12+1024], rbp" $
    encode (mov (derefOffset r12 1024) rbp)
      `shouldBe` [0x49,0x89,0xAC,0x24,0x00,0x04,0x00,0x00]
  it "mov qword [r13+1024], rsp" $
    encode (mov (derefOffset r13 1024) rsp)
      `shouldBe` [0x49,0x89,0xA5,0x00,0x04,0x00,0x00]
  it "mov qword [rbp+1024], r13" $
    encode (mov (derefOffset rbp 1024) r13)
      `shouldBe` [0x4C,0x89,0xAD,0x00,0x04,0x00,0x00]
  it "mov qword [rsp+1024], r12" $
    encode (mov (derefOffset rsp 1024) r12)
      `shouldBe` [0x4C,0x89,0xA4,0x24,0x00,0x04,0x00,0x00]
  it "mov qword [r12], r12" $
    encode (mov (derefOffset r12 0) r12)
      `shouldBe` [0x4D,0x89,0x24,0x24]
  it "mov qword [r13], r13" $
    encode (mov (derefOffset r13 0) r13)
      `shouldBe` [0x4D,0x89,0x6D,0x00]
  it "mov qword [rbp], rbp" $
    encode (mov (derefOffset rbp 0) rbp)
      `shouldBe` [0x48,0x89,0x6D,0x00]
  it "mov qword [rsp], rsp" $
    encode (mov (derefOffset rsp 0) rsp)
      `shouldBe` [0x48,0x89,0x24,0x24]
  it "mov qword [r12+1], r12" $
    encode (mov (derefOffset r12 1) r12)
      `shouldBe` [0x4D,0x89,0x64,0x24,0x01]
  it "mov qword [r13+1], r13" $
    encode (mov (derefOffset r13 1) r13)
      `shouldBe` [0x4D,0x89,0x6D,0x01]
  it "mov qword [rbp+1], rbp" $
    encode (mov (derefOffset rbp 1) rbp)
      `shouldBe` [0x48,0x89,0x6D,0x01]
  it "mov qword [rsp+1], rsp" $
    encode (mov (derefOffset rsp 1) rsp)
      `shouldBe` [0x48,0x89,0x64,0x24,0x01]
  it "mov qword [r12+1024], r12" $
    encode (mov (derefOffset r12 1024) r12)
      `shouldBe` [0x4D,0x89,0xA4,0x24,0x00,0x04,0x00,0x00]
  it "mov qword [r13+1024], r13" $
    encode (mov (derefOffset r13 1024) r13)
      `shouldBe` [0x4D,0x89,0xAD,0x00,0x04,0x00,0x00]
  it "mov qword [rbp+1024], rbp" $
    encode (mov (derefOffset rbp 1024) rbp)
      `shouldBe` [0x48,0x89,0xAD,0x00,0x04,0x00,0x00]
  it "mov qword [rsp+1024], rsp" $
    encode (mov (derefOffset rsp 1024) rsp)
      `shouldBe` [0x48,0x89,0xA4,0x24,0x00,0x04,0x00,0x00]
  it "mov rsp, rax" $
    encode (mov rsp rax)
      `shouldBe` [0x48,0x89,0xC4]
  it "mov rbp, rbx" $
    encode (mov rbp rbx)
      `shouldBe` [0x48,0x89,0xDD]
  it "mov rax, rsp" $
    encode (mov rax rsp)
      `shouldBe` [0x48,0x89,0xE0]
  it "mov rbx, rbp" $
    encode (mov rbx rbp)
      `shouldBe` [0x48,0x89,0xEB]
  it "mov [rsp], al" $
    encode (mov (derefOffset rsp 0) al)
      `shouldBe` [0x88,0x04,0x24]
  it "mov [rbp+1], al" $
    encode (mov (derefOffset rbp 1) al)
      `shouldBe` [0x88,0x45,0x01]
  it "mov [r12+1024], bl" $
    encode (mov (derefOffset r12 1024) bl)
      `shouldBe` [0x41,0x88,0x9C,0x24,0x00,0x04,0x00,0x00]
  it "mov [r13], bl" $
    encode (mov (derefOffset r13 0) bl)
      `shouldBe` [0x41,0x88,0x5D,0x00]
  it "mov al, [rsp]" $
    encode (mov al (derefOffset rsp 0))
      `shouldBe` [0x8A,0x04,0x24]
  it "mov al, [rbp+1]" $
    encode (mov al (derefOffset rbp 1))
      `shouldBe` [0x8A,0x45,0x01]
  it "mov bl, [r12+1024]" $
    encode (mov bl (derefOffset r12 1024))
      `shouldBe` [0x41,0x8A,0x9C,0x24,0x00,0x04,0x00,0x00]
  it "mov bl, [r13]" $
    encode (mov bl (derefOffset r13 0))
      `shouldBe` [0x41,0x8A,0x5D,0x00]
  it "mov bl, [rax+1024]" $
    encode (mov bl (derefOffset rax 1024))
      `shouldBe` [0x8A,0x98,0x00,0x04,0x00,0x00]
  it "mov bl, [rbx]" $
    encode (mov bl (derefOffset rbx 0))
      `shouldBe` [0x8A,0x1B]
  it "xor rbx, rbx" $
    encode (xor  rbx rbx)
      `shouldBe` [0x48,0x31,0xDB]
  it "xor rbx, rbx" $
    encode (xor  rbx rbx)
      `shouldBe` [0x48,0x31,0xDB]
  it "xor rax, rbx" $
    encode (xor  rax rbx)
      `shouldBe` [0x48,0x31,0xD8]
  it "xor rbx, rax" $
    encode (xor  rbx rax)
      `shouldBe` [0x48,0x31,0xC3]
  it "xor rbx, rax" $
    encode (xor  rbx rax)
      `shouldBe` [0x48,0x31,0xC3]
  it "xor rbx, r12" $
    encode (xor  rbx r12)
      `shouldBe` [0x4C,0x31,0xE3]
  it "xor r12, rax" $
    encode (xor  r12 rax)
      `shouldBe` [0x49,0x31,0xC4]
  it "push 0x33221100" $
    encode (push (I32 0x33221100))
      `shouldBe` [0x68,0x00,0x11,0x22,0x33]
  it "inc rdx" $
    encode (inc  rdx)
      `shouldBe` [0x48,0xFF,0xC2]
  it "inc rax" $
    encode (inc  rax)
      `shouldBe` [0x48,0xFF,0xC0]
  it "inc r9" $
    encode (inc  r9)
      `shouldBe` [0x49,0xFF,0xC1]
  it "test rax, rax" $
    encode (test rax rax)
      `shouldBe` [0x48,0x85,0xC0]
  it "test rbx, rdx" $
    encode (test rbx rdx)
      `shouldBe` [0x48,0x85,0xD3]
  it "test r9,  rdx" $
    encode (test r9 rdx)
      `shouldBe` [0x49,0x85,0xD1]
  it "pop rbx" $
    encode (pop  rbx)
      `shouldBe` [0x5B]
  it "pop r9" $
    encode (pop  r9)
      `shouldBe` [0x41,0x59]
  it "ret" $
    encode (ret)
      `shouldBe` [0xC3]
  it "call rbx" $
    encode (call rbx)
      `shouldBe` [0xFF,0xD3]
  it "call rax" $
    encode (call rax)
      `shouldBe` [0xFF,0xD0]
  it "call r9" $
    encode (call r9)
      `shouldBe` [0x41,0xFF,0xD1]
  it "add rbx, rdx" $
    encode (add  rbx rdx)
      `shouldBe` [0x48,0x01,0xD3]
  it "add rbx, r9" $
    encode (add  rbx r9)
      `shouldBe` [0x4C,0x01,0xCB]
  it "add r9, rbx" $
    encode (add  r9 rbx)
      `shouldBe` [0x49,0x01,0xD9]
  it "add r9, r9" $
    encode (add  r9 r9)
      `shouldBe` [0x4D,0x01,0xC9]
  it "push rbp" $
    encode (push rbp)
      `shouldBe` [0x55]
  it "push rax" $
    encode (push rax)
      `shouldBe` [0x50]
  it "push r12" $
    encode (push r12)
      `shouldBe` [0x41,0x54]
  it "sub rdx, 0x33221100" $
    encode (sub rdx (I32 0x33221100))
      `shouldBe` [0x48,0x81,0xEA,0x00,0x11,0x22,0x33]
  it "add rdx, 0x33221100" $
    encode (add rdx (I32 0x33221100))
      `shouldBe` [0x48,0x81,0xC2,0x00,0x11,0x22,0x33]
  it "add r8,  0x33221100" $
    encode (add r8  (I32 0x33221100))
      `shouldBe` [0x49,0x81,0xC0,0x00,0x11,0x22,0x33]
  it "add rdx, [rbp]" $
    encode (add rdx (rbp `derefOffset` 0))
      `shouldBe` [0x48,0x03,0x55,0x00]
  it "add rdx, [rbp - 128]" $
    encode (add rdx (rbp `derefOffset` (-128)))
      `shouldBe` [0x48,0x03,0x55,0x80]
  it "add rdx, [rbp + 127]" $
    encode (add rdx (rbp `derefOffset` 127))
      `shouldBe` [0x48,0x03,0x55,0x7F]
  it "sub rdx, [rbp]" $
    encode (sub rdx (rbp `derefOffset` 0))
      `shouldBe` [0x48,0x2B,0x55,0x00]
  it "sub rdx, [rbp - 128]" $
    encode (sub rdx (rbp `derefOffset` (-128)))
      `shouldBe` [0x48,0x2B,0x55,0x80]
  it "sub rdx, [rbp + 127]" $
    encode (sub rdx (rbp `derefOffset` 127))
      `shouldBe` [0x48,0x2B,0x55,0x7F]
  it "sub rdx, rbx" $
    encode (sub rdx rbx)
      `shouldBe` [0x48,0x29,0xDA]
  it "jmp [rbp]" $
    encode (jmpPtrOffset8  rbp 0)
      `shouldBe` [0xFF,0x65,0x00]
  it "jmp [rdi + 1]" $
    encode (jmpPtrOffset8  rdi 1)
      `shouldBe` [0xFF,0x67,0x01]
  it "jmp [r11 + 1]" $
    encode (jmpPtrOffset8  r11 1)
      `shouldBe` [0x41,0xFF,0x63,0x01]
  it "mov [rdi - 128], rdx" $
    encode (mov (rdi `derefOffset` (-128)) rdx)
      `shouldBe` [0x48,0x89,0x57,0x80]
  it "mov [rdi - 25600], rdx" $
    encode (mov (rdi `derefOffset` (-25600)) rdx)
      `shouldBe` [0x48,0x89,0x97,0x00,0x9C,0xFF,0xFF]
  it "cmp rbx, 0x33221100" $
    encode (cmp rbx (I32 0x33221100))
      `shouldBe` [0x48,0x81,0xFB,0x00,0x11,0x22,0x33]
  it "cmp rbx, rdx" $
    encode (cmp rbx rdx)
      `shouldBe` [0x48,0x39,0xD3]
  it "cmp rax, 0" $
    encode (cmp rax (I32 0))
      `shouldBe` [0x48,0x3D,0x00,0x00,0x00,0x00]
  it "mul rbx" $
    encode (mul rbx)
      `shouldBe` [0x48,0xF7,0xE3]
  it "mul rax" $
    encode (mul rax)
      `shouldBe` [0x48,0xF7,0xE0]
  it "mul r11" $
    encode (mul r11)
      `shouldBe` [0x49,0xF7,0xE3]
  it "sub rax, 1" $
    encode (sub rax (I32 1))
      `shouldBe` [0x48,0x2D,0x01,0x00,0x00,0x00]
  it "add rax, 1" $
    encode (add rax (I32 1))
      `shouldBe` [0x48,0x05,0x01,0x00,0x00,0x00]
  it "jmp rax" $
    encode (jmp rax)
      `shouldBe` [0xFF,0xE0]
  it "jmp rbx" $
    encode (jmp rbx)
      `shouldBe` [0xFF,0xE3]
  it "jmp r10" $
    encode (jmp r10)
      `shouldBe` [0x41,0xFF,0xE2]
  it "sal rax, 1" $
    encode (sal rax (I8 1))
      `shouldBe` [0x48,0xD1,0xE0]
  it "sal rbx, 1" $
    encode (sal rbx (I8 1))
      `shouldBe` [0x48,0xD1,0xE3]
  it "sal rbx, 2" $
    encode (sal rbx (I8 2))
      `shouldBe` [0x48,0xC1,0xE3,0x02]
  it "sal rbx, cl" $
    encode (sal rbx cl)
      `shouldBe` [0x48,0xD3,0xE3]
  it "sal rdx, cl" $
    encode (sal rdx cl)
      `shouldBe` [0x48,0xD3,0xE2]
  it "sar rax, 1" $
    encode (sar rax (I8 1))
      `shouldBe` [0x48,0xD1,0xF8]
  it "sar rbx, 1" $
    encode (sar rbx (I8 1))
      `shouldBe` [0x48,0xD1,0xFB]
  it "sar rbx, 2" $
    encode (sar rbx (I8 2))
      `shouldBe` [0x48,0xC1,0xFB,02]
  it "sar rbx, cl" $
    encode (sar rbx cl)
      `shouldBe` [0x48,0xD3,0xFB]
  it "sar rdx, cl" $
    encode (sar rdx cl)
      `shouldBe` [0x48,0xD3,0xFA]
  it "sal r9,      cl" $
    encode (sal r9 cl)
      `shouldBe` [0x49,0xD3,0xE1]
  it "sal r9,      2" $
    encode (sal r9 (I8 2))
      `shouldBe` [0x49,0xC1,0xE1,0x02]
  it "and rdx,  [rbp]" $
    encode (and_ rdx (rbp `derefOffset` 0))
      `shouldBe` [0x48,0x23,0x55,0x00]
  it "and rdx,  rbx" $
    encode (and_ rdx rbx)
      `shouldBe` [0x48,0x21,0xDA]
  it "and rdx,  9" $
    encode (and_ rdx (I32 9))
      `shouldBe` [0x48,0x81,0xE2,0x09,0x00,0x00,0x00]
  it "and rax,  9" $
    encode (and_ rax (I32 9))
      `shouldBe` [0x48,0x25,0x09,0x00,0x00,0x00]
  it "pop rax" $
    encode (pop  rax)
      `shouldBe` [0x58]
  it "dec rsi" $
    encode (dec rsi)
      `shouldBe` [0x48,0xFF,0xCE]
  it "dec r11" $
    encode (dec r11)
      `shouldBe` [0x49,0xFF,0xCB]
  it "dec rax" $
    encode (dec rax)
      `shouldBe` [0x48,0xFF,0xC8]
  it "mov r11, rsi" $
    encode (mov r11 rsi)
      `shouldBe` [0x49,0x89,0xF3]
  it "add rsp, [r9+24]" $
    encode (add rsp (derefOffset r9 24))
      `shouldBe` [0x49,0x03,0x61,0x18]
  it "add rax, [r9+24]" $
    encode (add rax (derefOffset r9 24))
      `shouldBe` [0x49,0x03,0x41,0x18]
  it "mov [rax+8], bl" $
    encode (mov (derefOffset rax 8) bl)
      `shouldBe` [0x88,0x58,0x08]
  it "mov [rsp+8], bl" $
    encode (mov (derefOffset rsp 8) bl)
      `shouldBe` [0x88,0x5C,0x24,0x08]
  it "mov byte [rsi+1], 0xAB" $
    encode (mov (derefOffset rsi 1) (I8 0xAB))
      `shouldBe` [0xC6,0x46,0x01,0xAB]
  it "setz bl" $
    encode (setz bl)
      `shouldBe` [0x0F,0x94,0xC3]
