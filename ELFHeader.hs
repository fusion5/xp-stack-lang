module ELFHeader where

import Data.Word

import ASM.ASM
import ASM.Datatypes

-- https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
elf64Header :: ASM () -> ASM ()
elf64Header body = do
    setLabel "begin_file"
    setLabel "begin_elf64_head"
    e_ident
    e_type
    e_machine
    e_version
    e_entry
    e_phoff
    e_shoff
    e_flags
    e_ehsize
    e_phentsize
    e_phnum
    e_shentsize
    e_shnum
    e_shstrndx
    assertOffsetIs 0x40 "The 64-bit header length has length 0x40"
    setLabel "end_elf64_head"
    body
    setLabel "end_file"
    where 
        e_ident = emit [
          0x7F, 0x45, 0x4C, 0x46, -- Magic byte + ELF in ascii
          0x02, -- EI_CLASS: 64 bit format
          0x01, -- EI_DATA: little endian
          0x01, -- EI_VERSION
          0x00, -- EI_OSABI
          0x00, -- EI_ABIVERSION
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 -- EI_PAD
          ]
        e_type      = emit [0x02, 0x00] -- ET_EXEC
        e_machine   = emit [0x3e, 0x00] -- x86
        e_version   = emit [0x01, 0x00, 0x00, 0x00] -- v1
        e_entry     = emit [0x78, 0x00, 0x00, 0xC0, 
                            0x00, 0x00, 0x00, 0x00]
        e_phoff     = emit [0x40, 0x00, 0x00, 0x00, -- Program header offset (size of EFI Header Length)
                            0x00, 0x00, 0x00, 0x00]
        e_shoff     = emit [0x00, 0x00, 0x00, 0x00, -- Section headers offset
                            0x00, 0x00, 0x00, 0x00]
        e_flags     = emit [0x00, 0x00, 0x00, 0x00]
        e_ehsize    = emit [0x40, 0x00] -- ELF header size
        e_phentsize = emit [0x38, 0x00] -- Program header size
        e_phnum     = emit [0x01, 0x00]
        e_shentsize = emit [0x00, 0x00]
        e_shnum     = emit [0x00, 0x00]
        e_shstrndx  = emit [0x00, 0x00]

programHeader :: Word64 -> ASM () -> ASM ()
programHeader 
    mem_offset  -- The program is loaded in memory at a certain offset.
    program =   -- The program
    do
        p_type
        p_flags
        p_offset
        p_vaddr
        p_paddr
        p_filesz
        p_memsz
        p_align
        assertOffsetIs (0x40 + 0x38) "64-bit program header offset (after ELF header)"
        setLabel "begin_program"
        program
        setLabel "end_program"
    where
        p_type   = emit [0x01, 0x00, 0x00, 0x00]
        p_flags  = emit [0x05, 0x00, 0x00, 0x00]
        p_offset = emit [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        p_vaddr  = emitWord64LE mem_offset
        p_paddr  = emitWord64LE mem_offset
        -- p_vaddr  = emit [0x00, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x00]
        -- p_paddr  = emit [0x00, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x00]
        p_filesz = emitProgSize64
        p_memsz  = emitProgSize64
        p_align  = emit [0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

