module X64.TestUtils where

import Data.List (intercalate)
import Data.Word
import qualified Data.ByteString.Lazy as BS

import X64.Types
import X64.X64
import ASM.ASM
import ASM.Types
import ASM.Pretty
import Debug.Trace
import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import Text.Printf

x64TestArray :: [(X64 (), String, [Word8])]
x64TestArray = [
    {- Haskell assembly -}             {- Respective NASM command -}
                                       {- Expected ByteString -}
    (label "TEST"                      , ".TEST"
                                       ,  [])
  , (mov  rax (I64 0x7766554433221100) , "mov rax, 0x7766554433221100"
                                       , [0x48,0xB8,0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77])
  , (mov  r11 (I64 0x7766554433221100) , "mov r11, 0x7766554433221100"
                                       , [0x49,0xBB,0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77])
  , (mov  rbx (I64 0x2A)               , "mov rbx, 0x2A"
                                       , [0x48,0xBB,0x2A,0x00,0x00,0x00,0x00,0x00,0x00,0x00])
  , (mov  rsp rcx                      , "mov rsp, rcx"
                                       , [0x48,0x89,0xCC])

  , (xor  rbx rbx                      , "xor rbx, rbx"
                                       , [0x48,0x31,0xDB])
  , (xor  rax rbx                      , "xor rax, rbx"
                                       , [0x48,0x31,0xD8])
  , (xor  rbx rax                      , "xor rbx, rax"
                                       , [0x48,0x31,0xC3])
  , (xor  rbx r12                      , "xor rbx, r12"
                                       , [0x4C,0x31,0xE3])
  , (xor  r12 rax                      , "xor r12, rax"
                                       , [0x49,0x31,0xC4])
  , (push (I32 0x33221100)             , "push 0x33221100"
                                       , [0x68,0x00,0x11,0x22,0x33])
  , (inc  rdx                          , "inc rdx"
                                       , [0x48,0xFF,0xC2])
  , (inc  rax                          , "inc rax"
                                       , [0x48,0xFF,0xC0])
  , (inc  r9                           , "inc r9"
                                       , [0x49,0xFF,0xC1])
  , (test rax rax                      , "test rax, rax"
                                       , [0x48,0x85,0xC0])
  , (test rbx rdx                      , "test rbx, rdx"
                                       , [0x48,0x85,0xD3])
  , (test r9 rdx                       , "test r9,  rdx"
                                       , [0x49,0x85,0xD1])
  , (pop  rbx                          , "pop rbx"
                                       , [0x5B])
  , (pop  r9                           , "pop r9"
                                       , [0x41,0x59])
  , (ret                               , "ret"
                                       , [0xC3])
  , (call rbx                          , "call rbx"
                                       , [0xFF,0xD3])
  , (call rax                          , "call rax"
                                       , [0xFF,0xD0])
  , (call r9                           , "call r9"
                                       , [0x41,0xFF,0xD1])
  , (add  rbx rdx                      , "add rbx, rdx"
                                       , [0x48,0x01,0xD3])
  , (add  rbx r9                       , "add rbx, r9"
                                       , [0x4C,0x01,0xCB])
  , (add  r9 rbx                       , "add r9, rbx"
                                       , [0x49,0x01,0xD9])
  , (add  r9 r9                        , "add r9, r9"
                                       , [0x4D,0x01,0xC9])
  , (push rbp                          , "push rbp"
                                       , [0x55])
  , (push rax                               , "push rax"
                                            , [0x50])
  , (push r12                               , "push r12"
                                            , [0x41,0x54])
  , (sub rdx (I32 0x33221100)               , "sub rdx, 0x33221100"
                                            , [0x48,0x81,0xEA,0x00,0x11,0x22,0x33])
  , (add rdx (I32 0x33221100)               , "add rdx, 0x33221100"
                                            , [0x48,0x81,0xC2,0x00,0x11,0x22,0x33])
  , (add r8  (I32 0x33221100)               , "add r8,  0x33221100"
                                            , [0x49,0x81,0xC0,0x00,0x11,0x22,0x33])
  , (mov rdx (rbp `derefOffset` 0)          , "mov rdx, [rbp]"
                                            , [0x48,0x8B,0x55,0x00])
  , (mov rax (rbp `derefOffset` 0)          , "mov rax, [rbp]"
                                            , [0x48,0x8B,0x45,0x00])
  , (mov rdx (rdi `derefOffset` (-32768))   , "mov rdx, [rdi - 32768]"
                                            , [0x48,0x8B,0x97,0x00,0x80,0xFF,0xFF])
  , (mov rdx (rdi `derefOffset` (-32767))   , "mov rdx, [rdi - 32767]"
                                            , [0x48,0x8B,0x97,0x01,0x80,0xFF,0xFF])
  , (mov rdx (rdi `derefOffset` (-129))     , "mov rdx, [rdi - 129]"
                                            , [0x48,0x8B,0x97,0x7F,0xFF,0xFF,0xFF])
  , (mov rdx (rdi `derefOffset` (-128))     , "mov rdx, [rdi - 128]"
                                            , [0x48,0x8B,0x57,0x80])
  , (mov rdx (rdi `derefOffset` (-127))     , "mov rdx, [rdi - 127]"
                                            , [0x48,0x8B,0x57,0x81])
  , (mov rdx (rdi `derefOffset` 127)        , "mov rdx, [rdi + 127]"
                                            , [0x48,0x8B,0x57,0x7F])
  , (mov rdx (rdi `derefOffset` 128)        , "mov rdx, [rdi + 128]"
                                            , [0x48,0x8B,0x97,0x80,0x00,0x00,0x00])
  , (mov rdx (rdi `derefOffset` 129)        , "mov rdx, [rdi + 129]"
                                            , [0x48,0x8B,0x97,0x81,0x00,0x00,0x00])
  , (mov rdx (rdi `derefOffset` (32768))    , "mov rdx, [rdi + 32768]"
                                            , [0x48,0x8B,0x97,0x00,0x80,0x00,0x00])
  , (add rdx (rbp `derefOffset` 0)          , "add rdx, [rbp]"
                                            , [0x48,0x03,0x55,0x00])
  , (add rdx (rbp `derefOffset` (-128))     , "add rdx, [rbp - 128]"
                                            , [0x48,0x03,0x55,0x80])
  , (add rdx (rbp `derefOffset` 127)        , "add rdx, [rbp + 127]"
                                            , [0x48,0x03,0x55,0x7F])
  , (sub rdx (rbp `derefOffset` 0)          , "sub rdx, [rbp]"
                                            , [0x48,0x2B,0x55,0x00])
  , (sub rdx (rbp `derefOffset` (-128))     , "sub rdx, [rbp - 128]"
                                            , [0x48,0x2B,0x55,0x80])
  , (sub rdx (rbp `derefOffset` 127)        , "sub rdx, [rbp + 127]"
                                            , [0x48,0x2B,0x55,0x7F])
  , (sub rdx rbx                            , "sub rdx, rbx"
                                            , [0x48,0x29,0xDA])
  , (jmpPtrOffset8  rbp 0                   , "jmp [rbp]"
                                            , [0xFF,0x65,0x00])
  , (jmpPtrOffset8  rdi 1                   , "jmp [rdi + 1]"
                                            , [0xFF,0x67,0x01])
  , (jmpPtrOffset8  r11 1                   , "jmp [r11 + 1]"
                                            , [0x41,0xFF,0x63,0x01])
  , (mov (rdi `derefOffset` (-128)) rdx     , "mov [rdi - 128], rdx"
                                            , [0x48,0x89,0x57,0x80])
  , (mov (rdi `derefOffset` (-25600)) rdx   , "mov [rdi - 25600], rdx"
                                            , [0x48,0x89,0x97,0x00,0x9C,0xFF,0xFF])
  , (cmp rbx (I32 0x33221100)               , "cmp rbx, 0x33221100"
                                            , [0x48,0x81,0xFB,0x00,0x11,0x22,0x33])
  , (cmp rbx rdx                            , "cmp rbx, rdx"
                                            , [0x48,0x39,0xD3])
  , (cmp rax (I32 0)                        , "cmp rax, 0"
                                            , [0x48,0x3D,0x00,0x00,0x00,0x00])
  , (mul rbx                                , "mul rbx"
                                            , [0x48,0xF7,0xE3])
  , (mul rax                                , "mul rax"
                                            , [0x48,0xF7,0xE0])
  , (mul r11                                , "mul r11"
                                            , [0x49,0xF7,0xE3])
  , (sub rax (I32 1)                        , "sub rax, 1"
                                            , [0x48,0x2D,0x01,0x00,0x00,0x00])
  , (add rax (I32 1)                        , "add rax, 1"
                                            , [0x48,0x05,0x01,0x00,0x00,0x00])
  , (jmp rax                                , "jmp rax"
                                            , [0xFF,0xE0])
  , (jmp rbx                                , "jmp rbx"
                                            , [0xFF,0xE3])
  , (jmp r10                                , "jmp r10"
                                            , [0x41,0xFF,0xE2])
  , (mov (rbp `derefOffset` 127) (I32 32)   , "mov qword [rbp + 127], 32"
                                            , [0x48,0xC7,0x45,0x7F,0x20,0x00,0x00,0x00])
  , (mov (rbp `derefOffset` 128) (I32 32)   , "mov qword [rbp + 128], 32"
                                            , [0x48,0xC7,0x85,0x80,0x00,0x00,0x00,0x20,0x00,0x00,0x00])
  , (mov (rbp `derefOffset` 127) (I8  0x48) , "mov byte  [rbp + 127], 0x48"
                                            , [0xC6,0x45,0x7F,0x48])
  , (mov (rbp `derefOffset` 0)   (I8  0xEA) , "mov byte  [rbp],   0xEA"
                                            , [0xC6,0x45,0x00,0xEA])
  , (mov (RL8 BX) (rdi `derefOffset` 1)      , "mov byte  bl, [rdi+1]"
                                            , [0x8A,0x5F,0x01])
  , (mov (RL8 AX) (rdi `derefOffset` 0)      , "mov byte  al, [rdi]"
                                            , [0x8A,0x07])
  , (mov (RL8 CX) (rcx `derefOffset` 0)      , "mov byte  cl, [rcx]"
                                            , [0x8A,0x09])
  , (sal rax (I8 1)                         , "sal rax, 1"
                                            , [0x48,0xD1,0xE0])
  , (sal rbx (I8 1)                         , "sal rbx, 1"
                                            , [0x48,0xD1,0xE3])
  , (sal rbx (I8 2)                         , "sal rbx, 2"
                                            , [0x48,0xC1,0xE3,0x02])
  , (sal rbx cl                             , "sal rbx, cl"
                                            , [0x48,0xD3,0xE3])
  , (sal rdx cl                             , "sal rdx, cl"
                                            , [0x48,0xD3,0xE2])
  , (sar rax (I8 1)                         , "sar rax, 1"
                                            , [0x48,0xD1,0xF8])
  , (sar rbx (I8 1)                         , "sar rbx, 1"
                                            , [0x48,0xD1,0xFB])
  , (sar rbx (I8 2)                         , "sar rbx, 2"
                                            , [0x48,0xC1,0xFB,02])
  , (sar rbx cl                             , "sar rbx, cl"
                                            , [0x48,0xD3,0xFB])
  , (sar rdx cl                             , "sar rdx, cl"
                                            , [0x48,0xD3,0xFA])
  , (sal r9 cl                              , "sal r9,      cl"
                                            , [0x49,0xD3,0xE1])
  , (sal r9 (I8 2)                          , "sal r9,      2"
                                            , [0x49,0xC1,0xE1,0x02])
  , (mov (rbp `derefOffset` 64) cl          , "mov byte  [rbp + 64], cl"
                                            , [0x88,0x4D,0x40])
  , (mov rsi (rsi `derefOffset` 8)          , "mov qword rsi, [rsi + 8]"
                                            , [0x48,0x8B,0x76,0x08])
  , (mov rax (rsp `derefOffset` 8)          , "mov qword rax, [rsp + 8]"
                                            , [0x48,0x8B,0x44,0x24,0x08])
  , (mov (rsp `derefOffset` 8) rax          , "mov qword [rsp + 8],   rax"
                                            , [0x48,0x89,0x44,0x24,0x08])
  , (mov (rsp `derefOffset` 255) rbx        , "mov qword [rsp + 255], rbx"
                                            , [0x48,0x89,0x9C,0x24,0xFF,0x00,0x00,0x00])
  , (mov (rsp `derefOffset` 8) (I32 100)    , "mov qword [rsp + 8],   100"
                                            , [0x48,0xC7,0x44,0x24,0x08,0x64,0x00,0x00,0x00])
  , (mov (rsp `derefOffset` 255) (I32 100)  , "mov qword [rsp + 255], 100"
                                            , [0x48,0xC7,0x84,0x24,0xFF,0x00,0x00,0x00,0x64,0x00,0x00,0x00])
  , (mov (rsp `derefOffset` 255) (I8 0xEA)  , "mov byte  [rsp + 255], 0xEA"
                                            , [0xC6,0x84,0x24,0xFF,0x00,0x00,0x00,0xEA])
  , (mov (rsp `derefOffset` 8)   (I8 0xEA)  , "mov byte  [rsp + 8],   0xEA"
                                            , [0xC6,0x44,0x24,0x08,0xEA])
  , (mov (r9  `derefOffset` 8)   (I8 0xEA)  , "mov byte  [r9  + 8],   0xEA"
                                            , [0x41,0xC6,0x41,0x08,0xEA])
  , (mov (r9  `derefOffset` 8)   cl         , "mov byte  [r9  + 8],   cl"
                                            , [0x41,0x88,0x49,0x08])
  , (and_ rdx (rbp `derefOffset` 0)         , "and rdx,  [rbp]"
                                            , [0x48,0x23,0x55,0x00])
  , (and_ rdx rbx                           , "and rdx,  rbx"
                                            , [0x48,0x21,0xDA])
  , (and_ rdx (I32 9)                       , "and rdx,  9"
                                            , [0x48,0x81,0xE2,0x09,0x00,0x00,0x00])
  , (and_ rax (I32 9)                       , "and rax,  9"
                                            , [0x48,0x25,0x09,0x00,0x00,0x00])
  , (pop  rax                               , "pop rax"
                                            , [0x58])
  , (mov (RL8 AX) (rsp `derefOffset` 0)      , "mov byte al, [rsp+0]"
                                            , [0x8A,0x04,0x24])
  , (mov (RL8 AX) (rcx `derefOffset` 0)      , "mov byte al, [rcx+0]"
                                            , [0x8A,0x01])
  , (mov rax (derefOffset r9 1)             , "mov rax,     [r9+1]"
                                            , [0x49,0x8B,0x41,0x01])
  , (mov rax (derefOffset rax 1)            , "mov rax,     [rax+1]"
                                            , [0x48,0x8B,0x40,0x01])
  , (mov rax (derefOffset rsi 0)            , "mov rax,     [rsi]"
                                            , [0x48,0x8B,0x06])
  , (mov r9 (derefOffset rax 1)             , "mov r9,      [rax+1]"
                                            , [0x4C,0x8B,0x48,0x01])
  , (mov r9 (derefOffset r8 1)              , "mov r9,      [r8+1]"
                                            , [0x4D,0x8B,0x48,0x01])
  , (mov r9 (derefOffset rax 0)             , "mov r9,      [rax]"
                                            , [0x4C,0x8B,0x08])
  , (mov r9 (derefOffset r8 0)              , "mov r9,      [r8]"
                                            , [0x4D,0x8B,0x08])
  , (mov rsp (derefOffset r8 0)             , "mov rsp,     [r8]"
                                            , [0x49,0x8B,0x20])
  , (mov r9 (derefOffset rsp 0)             , "mov r9,      [rsp]"
                                            , [0x4C,0x8B,0x0C,0x24])
  , (mov (derefOffset rsi 0) r15            , "mov qword [rsi], r15"
                                            , [0x4C,0x89,0x3E])
  , (mov (derefOffset rsp 0) r15            , "mov qword [rsp], r15"
                                            , [0x4C,0x89,0x3C,0x24])
  , (mov (derefOffset r11 0) rax            , "mov qword [r11], rax"
                                            , [0x49,0x89,0x03])
  , (mov (derefOffset r11 0) rsp            , "mov qword [r11], rsp"
                                            , [0x49,0x89,0x23])
  , (mov (derefOffset rsi 1) r15            , "mov qword [rsi + 1], r15"
                                            , [0x4C,0x89,0x7E,0x01])
  , (mov (derefOffset rsp 1) r15            , "mov qword [rsp + 1], r15"
                                            , [0x4C,0x89,0x7C,0x24,0x01])
  , (mov (derefOffset r11 1) rax            , "mov qword [r11 + 1], rax"
                                            , [0x49,0x89,0x43,0x01])
  , (mov (derefOffset r11 1) rsp            , "mov qword [r11 + 1], rsp"
                                            , [0x49,0x89,0x63,0x01])
  , (mov (derefOffset rsi 1) rax            , "mov qword [rsi + 1], rax"
                                            , [0x48,0x89,0x46,0x01])
  , (mov (derefOffset rsi 1024) r15         , "mov qword [rsi + 1024], r15"
                                            , [0x4C,0x89,0xBE,0x00,0x04,0x00,0x00])
  , (mov (derefOffset rsp 1024) r15         , "mov qword [rsp + 1024], r15"
                                            , [0x4C,0x89,0xBC,0x24,0x00,0x04,0x00,0x00])
  , (mov (derefOffset r11 1024) rax         , "mov qword [r11 + 1024], rax"
                                            , [0x49,0x89,0x83,0x00,0x04,0x00,0x00])
  , (mov (derefOffset r11 1024) rsp         , "mov qword [r11 + 1024], rsp"
                                            , [0x49,0x89,0xA3,0x00,0x04,0x00,0x00])
  , (mov (derefOffset r11 1024) (I32 0x100) , "mov qword [r11 + 1024], 0x100"
                                            , [0x49,0xC7,0x83,0x00,0x04,0x00,0x00,0x00,0x01,0x00,0x00])
  , (mov (derefOffset r11 64)   (I32 0x100) , "mov qword [r11 + 64],   0x100"
                                            , [0x49,0xC7,0x43,0x40,0x00,0x01,0x00,0x00])
  , (dec rsi                                , "dec rsi"
                                            , [0x48,0xFF,0xCE])
  , (dec r11                                , "dec r11"
                                            , [0x49,0xFF,0xCB])
  , (dec rax                                , "dec rax"
                                            , [0x48,0xFF,0xC8])
  , (mov r11 rsi                            , "mov r11, rsi"
                                            , [0x49,0x89,0xF3])
  , (add rsp (derefOffset r9 24)            , "add rsp, [r9+24]"
                                            , [0x49,0x03,0x61,0x18])
  , (add rax (derefOffset r9 24)            , "add rax, [r9+24]"
                                            , [0x49,0x03,0x41,0x18])
  , (mov (derefOffset rax 8) bl             , "mov [rax+8], bl"
                                            , [0x88,0x58,0x08])
  , (mov (derefOffset rsp 8) bl             , "mov [rsp+8], bl"
                                            , [0x88,0x5C,0x24,0x08])
  , (mov (derefOffset rsi 1) (I8 0xAB)      , "mov byte [rsi+1], 0xAB"
                                            , [0xC6,0x46,0x01,0xAB])
    -- Edge cases for MOV when used as base indices, i.e. as the first
    -- argument to derefOffset: rbp, rsp, r12, r13
  , (mov (derefOffset r12 0) rax            , "mov qword [r12], rax",   [0x49,0x89,0x04,0x24])
  , (mov (derefOffset r13 0) rax            , "mov qword [r13], rax",   [0x49,0x89,0x45,0x00])
  , (mov (derefOffset rbp 0) rax            , "mov qword [rbp], rax",   [0x48,0x89,0x45,0x00])
  , (mov (derefOffset rsp 0) rax            , "mov qword [rsp], rax",   [0x48,0x89,0x04,0x24])
  , (mov (derefOffset r12 0) rbp            , "mov qword [r12], rbp",   [0x49,0x89,0x2C,0x24])
  , (mov (derefOffset r13 0) rsp            , "mov qword [r13], rsp",   [0x49,0x89,0x65,0x00])
  , (mov (derefOffset rbp 0) r13            , "mov qword [rbp], r13",   [0x4C,0x89,0x6D,0x00])
  , (mov (derefOffset rsp 0) r12            , "mov qword [rsp], r12",   [0x4C,0x89,0x24,0x24])
  , (mov (derefOffset r12 1) rbp            , "mov qword [r12+1], rbp", [0x49,0x89,0x6C,0x24,0x01])
  , (mov (derefOffset r13 1) rsp            , "mov qword [r13+1], rsp", [0x49,0x89,0x65,0x01])
  , (mov (derefOffset rbp 1) r13            , "mov qword [rbp+1], r13", [0x4C,0x89,0x6D,0x01])
  , (mov (derefOffset rsp 1) r12            , "mov qword [rsp+1], r12", [0x4C,0x89,0x64,0x24,0x01])
  , (mov (derefOffset r12 1024) rbp         , "mov qword [r12+1024], rbp", [0x49,0x89,0xAC,0x24,0x00,0x04,0x00,0x00])
  , (mov (derefOffset r13 1024) rsp         , "mov qword [r13+1024], rsp", [0x49,0x89,0xA5,0x00,0x04,0x00,0x00])
  , (mov (derefOffset rbp 1024) r13         , "mov qword [rbp+1024], r13", [0x4C,0x89,0xAD,0x00,0x04,0x00,0x00])
  , (mov (derefOffset rsp 1024) r12         , "mov qword [rsp+1024], r12", [0x4C,0x89,0xA4,0x24,0x00,0x04,0x00,0x00])

  , (mov (derefOffset r12 0) r12            , "mov qword [r12], r12",   [0x4D,0x89,0x24,0x24])
  , (mov (derefOffset r13 0) r13            , "mov qword [r13], r13",   [0x4D,0x89,0x6D,0x00])
  , (mov (derefOffset rbp 0) rbp            , "mov qword [rbp], rbp",   [0x48,0x89,0x6D,0x00])
  , (mov (derefOffset rsp 0) rsp            , "mov qword [rsp], rsp",   [0x48,0x89,0x24,0x24])

  , (mov (derefOffset r12 1) r12            , "mov qword [r12+1], r12",   [0x4D,0x89,0x64,0x24,0x01])
  , (mov (derefOffset r13 1) r13            , "mov qword [r13+1], r13",   [0x4D,0x89,0x6D,0x01])
  , (mov (derefOffset rbp 1) rbp            , "mov qword [rbp+1], rbp",   [0x48,0x89,0x6D,0x01])
  , (mov (derefOffset rsp 1) rsp            , "mov qword [rsp+1], rsp",   [0x48,0x89,0x64,0x24,0x01])

  , (mov (derefOffset r12 1024) r12         , "mov qword [r12+1024], r12",   [0x4D,0x89,0xA4,0x24,0x00,0x04,0x00,0x00])
  , (mov (derefOffset r13 1024) r13         , "mov qword [r13+1024], r13",   [0x4D,0x89,0xAD,0x00,0x04,0x00,0x00])
  , (mov (derefOffset rbp 1024) rbp         , "mov qword [rbp+1024], rbp",   [0x48,0x89,0xAD,0x00,0x04,0x00,0x00])
  , (mov (derefOffset rsp 1024) rsp         , "mov qword [rsp+1024], rsp",   [0x48,0x89,0xA4,0x24,0x00,0x04,0x00,0x00])
  , (mov rsp rax                            , "mov rsp, rax",   [0x48,0x89,0xC4])
  , (mov rbp rbx                            , "mov rbp, rbx",   [0x48,0x89,0xDD])
  , (mov rax rsp                            , "mov rax, rsp",   [0x48,0x89,0xE0])
  , (mov rbx rbp                            , "mov rbx, rbp",   [0x48,0x89,0xEB])

  , (mov (derefOffset rsp 0) al             , "mov [rsp], al",      [0x88,0x04,0x24])
  , (mov (derefOffset rbp 1) al             , "mov [rbp+1], al",    [0x88,0x45,0x01])
  , (mov (derefOffset r12 1024) bl          , "mov [r12+1024], bl", [0x41,0x88,0x9C,0x24,0x00,0x04,0x00,0x00])
  , (mov (derefOffset r13 0) bl             , "mov [r13], bl",      [0x41,0x88,0x5D,0x00])

  , (mov al (derefOffset rsp 0)             , "mov al, [rsp]",      [0x8A,0x04,0x24])
  , (mov al (derefOffset rbp 1)             , "mov al, [rbp+1]",    [0x8A,0x45,0x01])
  , (mov bl (derefOffset r12 1024)          , "mov bl, [r12+1024]", [0x41,0x8A,0x9C,0x24,0x00,0x04,0x00,0x00])
  , (mov bl (derefOffset r13 0)             , "mov bl, [r13]",      [0x41,0x8A,0x5D,0x00])
  , (mov bl (derefOffset rax 1024)          , "mov bl, [rax+1024]", [0x8A,0x98,0x00,0x04,0x00,0x00])
  , (mov bl (derefOffset rbx 0)             , "mov bl, [rbx]",      [0x8A,0x1B])


  , (setz bl                                , "setz bl" , [0x0F,0x94,0xC3])
  -- In Windows we need to do this to get the TEB (Thread Environment Block)
  -- , (dec rbx                                   , "mov ebx, [gs:0x30]")
  -- just some quick tests
  -- , (mov (derefOffset rsi 0) (I32 0x3)         , "mov dword [rsi+0], 0x3")
  -- , (mov (derefOffset rsi 4) (I32 0x3)         , "mov dword [rsi+4], 0x3")
  -- , (joNear "TEST"                             , "jo .TEST")
  ]

x64TestSuiteExpected = map (\(_, _, x) -> BS.pack x) x64TestArray
x64TestSuiteASM  = runX64 $ mapM (\(x, _, _) -> x) x64TestArray
x64TestSuiteNASM = intercalate "\n" $ map (\(_, x, _) -> x) x64TestArray

x64TestResults x = filter ((/=) []) (zipWith x64TestResult x64TestArray x)
  where
    -- f (x, y, z) got expected | got == expected = "PASS: " ++ y

x64TestResult :: (X64 (), String, [Word8]) -> BS.ByteString -> String
x64TestResult (x, cmd, expected) got | got /= BS.pack expected =
    -- trace cmd $
        "FAIL <<" ++ cmd ++ ">> \n" ++
        "  Got result: " ++ dumpByteString got ++ " " ++ "\n" ++
        "  Expected:   " ++ dumpByteString (BS.pack expected) ++ "\n" ++
        "  Got binary: " ++ got_bindig ++ "\n" ++
        "  Exp binary: " ++ exp_bindig
  where got_bindig = concatMap (\x -> printf "%08b " x) (BS.unpack got)
        exp_bindig = concatMap (\x -> printf "%08b " x) expected
x64TestResult (_, cmd, _) _ =
    -- trace cmd $
        ""

