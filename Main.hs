{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment

import ASM.Datatypes        -- Common datatypes
import ASM.ASM
import ASM.Pretty       -- Print ASM with nice comments (documentation)
import ELFHeader64      -- ELF header
import PEHeader64       -- ELF header
import X86.Datatypes    -- X86-specific assembly datatypes
import X86.X86          -- X86-specific assembly code
import X86.Tests        -- X86-specific test suite (with NASM commands)

import Data.Word

import Lang.Lang
import Lang.Debug
import Lang.Linux
import Lang.BasicFunctions
import Lang.Datatypes

import Data.Bits

import Data.Maybe (listToMaybe)
import Data.List  (intercalate)

import Lang.ParserTests (testSuiteParsers, parserTestSuiteStdin)
import qualified Data.ByteString as BS

pstack_size_bytes = 10000
reg_pstack        = rsi

initParamStack :: Platform -> X86_64 ()
initParamStack Linux = do
    docX86 "Initialise the heap. brk(), the Linux system call for this"
    docX86 "has id number 45. We use the heap as a function parameter"
    docX86 "stack (pstack), i.e. it is used to pass parameters to functions."
    mov rax (I64 linux_sys_brk)
    mov rbx (I64 0)
    int

    docX86 "rax holds the program break address."
    mov reg_pstack rax 

    docX86 "A second brk() call is needed."
    mov rax (I64 linux_sys_brk) 
    docX86 "Set the heap start address as brk argument"
    mov rbx reg_pstack
    docX86 "Allocate 10k bytes for the pstack."
    add rbx (I32 pstack_size_bytes)
    int
    -- FIXME: If the heap alloc didn't work then this 
    -- should throw a segmentation fault.
    docX86 "The parameter stack grows from large to small, "
    docX86 "similar to the call stack. Therefore we start from "
    docX86 "the highest possible address."
    add reg_pstack (I32 pstack_size_bytes) 
    docX86 "Now, reg_pstack holds the pstack top."
initParamStack Windows = do
    docX86 "All Windows processes have kernel32.dll and ntdll.dll loaded"
    docX86 "in memory after the startup. To call one of them, we must"
    docX86 "first locate the kernel DLL using information from the PE header."
    docX86 "1. Find the DLL base address"

    -- mov ebx, fs -- register
    docX86 "Shadow space (x64 calling convention):"
    sub rsp (I32 32)
    mov rcx (I64 0)
    -- mov rdx (I32 pstack_size_bytes)
    -- mov r8  mem_commit
    -- mov r9  page_readwrite
    -- call VirtualAlloc 

-- TODO: Rename to initDictionaryBodiesLinux
initJITDefinitionsMemLinux :: X86_64 ()
initJITDefinitionsMemLinux = do
    docX86 "Allocate executable memory in which we place the body of "
    docX86 "definitions generated by terms parsed in the REPL (at runtime)."
    docX86 "This is also called JIT ASM code. R9 is the dedicated register."
    docX86 "Retrieve the current memory break:"
    mov rax (I64 linux_sys_brk)
    mov rbx (I64 0) -- An rbx of 0 means that brk() returns the program break.
    int
    docX86 "rax now holds the program break."
    mov r9  rax
    docX86 "r9 now holds the area where new just-in-time function definitions "
    docX86 "are placed."

    {-
    do
        -- TEMP DEBUG
        ppush r9
        callLabel "dbg_dump_ptop_w64"
        writeMsgHelper "JIT start\n"
        pdrop 1
    -}

    mov rbx rax -- An rbx of the program break means that we wish to extend the
                -- program break
    add rbx (I32 10000) -- by 10k bytes
    mov rax (I64 linux_sys_brk)
    int
    doc "We've successfully allocated the memory for definitions in r9."
    doc "Apparently it's writeable and executable by default."

initDictionaryMemLinux :: X86_64 ()
initDictionaryMemLinux = do
    doc "Allocate memory for the dictionary. The dictionary is a linked list"
    doc "With entries having fields [prev][hash][address], where:"
    doc "- prev is a pointer to the previous dictionary element."
    doc "- hash is the term hash."
    doc "- address is a pointer to the JIT definitions memory zone, the "
    doc "  definition contents."  
    mov rax (I64 linux_sys_brk)
    mov rbx (I64 0) -- An rbx of 0 means that brk() returns the program break.
    int
    doc "rax now holds the program break."
    mov r11 rax
    doc "r11 is our dictionary linked list end position pointer."
    
    mov rbx rax
    add rbx (I32 10000) -- 10k bytes for the dictionary linked list
    mov rax (I64 linux_sys_brk)
    int
    doc "We've successfully allocated memory for the dictionary linked list."
    doc "Add a first, dummy, empty term to mark the end of the list"
    mov (derefOffset r11 0)  (I32 0)
    mov (derefOffset r11 8)  (I32 0)
    mov (derefOffset r11 16) (I32 0)

mainParserTests platform = do
    doc "Run the parser test suite"
    mov rbp rsp
    initParamStack platform

    initDictionaryMemLinux
    initJITDefinitionsMemLinux

    baseDefEntries

    testSuiteParsers
    
    ppush $ I32 0
    callLabel "exit"

    baseDefBodies

bootstrapX86_64 platform = do
    doc "Initialise and start the language REPL."

    -- int3
    X86.X86.xor rax rax
    -- TODO: Find mov/call opcodes that to do this more neatly?
    mov rax (L64 "ExitProcess")
    mov rax (rax `derefOffset` 0)

    docX86 "Shadow space (x64 calling convention):"
    sub rsp (I32 32)
    mov rcx (I64 2129)
    {-
    mov rdx (I64 2129)
    mov r8  mem_commit
    mov r9  page_readwrite
    -}
    call rax

    ret


    {-
    mov rbp rsp
    initParamStack platform

    initDictionaryMemLinux
    initJITDefinitionsMemLinux

    baseDefEntries

    let mainTerm = "repl"
    ppushStr mainTerm
    ppushI32 $ length mainTerm
    callLabel "term_hash_look"
    assertPtop 1 "The 'repl' dictionary entry not found!\n"
    doc "Drop the success return code of TERM_LOOK:"
    pdrop 1
    doc "Now take the '.addr' field from the dictionary term found by "
    doc "term_hash_look:"
    ppop rax
    mov rax (derefOffset rax 16)

    doc "Evaluate the repl dictionary entry:"
    call rax

    -- mov rax $ I64 (2^64 - 1)
    -- ppush rax
    -- callLabel "dbg_dump_ptop_w64"
    callLabel "exit"

    baseDefBodies
    -}

assembly Linux bodyX86_64 = do
    elf64Header 
        (
            elf64ProgramHeader vaddr_offset 
                (
                    do
                        runX86 bodyX86_64
                        doc "The table of all collected strings:"
                        emitStringTable
                )
        )
    replaceProgSize -- TODO: Could be removed (by replacing it with label diffs)
    replaceLabels  vaddr_offset
    replaceStrRefs vaddr_offset
        where vaddr_offset = 0xC0000000
assembly Windows bodyX86_64 = do
    pe64Header image_base (
        do
            runX86 bodyX86_64
            doc "The table of all collected strings:"
            emitStringTable
        )
    replaceLabels  image_base
    replaceStrRefs image_base 
        where image_base = 0x400000

outputStr f as = 
    case assemble as of 
        Left err -> 
            putStrLn $ "Error: " ++ err
        Right s  -> 
            putStrLn $ f s

outputByteStr f as = 
    case assemble as of 
        Left err -> 
            putStrLn $ "Error: " ++ err
        Right s  -> 
            BS.putStr $ f s

writeByteStr fname f as = 
    case assemble as of 
        Left err -> 
            putStrLn $ "Error: " ++ err
        Right s  -> 
            BS.writeFile fname (f s)


main :: IO ()
main = do
    args <- getArgs
    case listToMaybe args of
        Nothing                 -> outputStr ASM.Pretty.asmHex $ 
                                        assembly Linux (bootstrapX86_64 Linux)
        Just "dump_bytes"       -> outputStr ASM.Pretty.asmHex $
                                        assembly Linux (bootstrapX86_64 Linux)
        Just "doc"              -> outputStr ASM.Pretty.asmDocs $
                                        assembly Linux (bootstrapX86_64 Linux)
        Just "bootstrap_x86_windows" 
                                -> outputStr ASM.Pretty.asmHex $
                                        assembly Windows (bootstrapX86_64 Windows)
        Just "bootstrap_x86_windows_bin" 
                                -> writeByteStr "main.exe" ASM.Pretty.asmBin $
                                        assembly Windows (bootstrapX86_64 Windows)
        Just "bootstrap_x86_windows_doc" 
                                -> outputStr ASM.Pretty.asmDocs $
                                        assembly Windows (bootstrapX86_64 Windows)

        Just "bootstrap_x86_linux" 
                                -> outputStr ASM.Pretty.asmHex $
                                        assembly Linux (bootstrapX86_64 Linux)
        -- Parser tests
        Just "test_par"         -> outputStr ASM.Pretty.asmHex $
                                        assembly Linux (mainParserTests Linux)
        Just "test_par_doc"     -> outputStr ASM.Pretty.asmDocs $
                                        assembly Linux (mainParserTests Linux)
        Just "test_par_stdin"   -> mapM putStr parserTestSuiteStdin >> return ()

        -- X86 opcode generation tests (Haskell vs. NASM)
        Just "test_x86"         -> outputStr     ASM.Pretty.asmHex x86TestSuiteASM
        Just "test_x86_bin"     -> outputByteStr ASM.Pretty.asmBin x86TestSuiteASM
        Just "test_x86_n"       -> putStr   $ x86TestSuiteNASM
        Just x                  -> putStrLn $ "Unknown option \"" ++ x ++ "\"" ++
                                 " of (dump_bytes, doc, test_x86, test_x86_n)"

