{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment

import ASM.Datatypes        -- Common datatypes
import ASM.ASM
import ASM.Pretty       -- Print ASM with nice comments (documentation)
import ELFHeader64      -- ELF header
-- import Linux         -- Linux constants
import X86.Datatypes    -- X86-specific assembly datatypes
import X86.X86          -- X86-specific assembly code
import X86.Tests        -- X86-specific test suite (with NASM commands)

import Data.Word

import Lang.Lang
import Lang.Debug
import Lang.Linux
import Lang.BasicFunctions

import Data.Bits

import Data.Maybe (listToMaybe)
import Data.List (intercalate)

import Lang.ParserTests (testSuiteParsers, parserTestSuiteStdin)

initParamStackLinux :: X86_64 ()
initParamStackLinux = do
    docX86 "Initialise the heap. brk(), the Linux system call for this"
    docX86 "has id number 45. We use the heap as a function parameter"
    docX86 "stack (pstack), i.e. it is used to pass parameters to functions."
    mov rax (I64 linux_sys_brk)
    mov rbx (I64 0)
    int

    docX86 "rax holds the program break address."
    mov rsi rax 

    docX86 "A second brk() call is needed."
    mov rax (I64 linux_sys_brk) 
    docX86 "Set the heap start address as brk argument"
    mov rbx rsi
    docX86 "Allocate 10k bytes for the pstack."
    add rbx (I32 10000)
    int
    -- FIXME: If the heap alloc didn't work then this 
    -- should throw a segmentation fault.
    docX86 "The parameter stack grows from large to small, "
    docX86 "similar to the call stack. Therefore we start from "
    docX86 "the highest possible address."
    add rsi (I32 10000) 
    docX86 "Now, rsi holds the pstack top."

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

mainTP = do
    doc "Run the parser test suite"
    mov rbp rsp
    initParamStackLinux

    initDictionaryMemLinux
    initJITDefinitionsMemLinux

    baseDefEntries

    testSuiteParsers
    
    ppush $ I32 0
    callLabel "exit"

    baseDefBodies

mainX86 = do
    doc "Initialise and start the language REPL."

    mov rbp rsp
    initParamStackLinux

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

assembly mainX86 = do
    elf64Header $ programHeader vaddr_offset $ do
        runX86 mainX86
        doc "The string table with all collected strings:"
        emitStringTable
    replaceProgSize
    replaceLabels  vaddr_offset
    replaceStrRefs vaddr_offset
        where vaddr_offset = 0xC0000000

doAction f as = 
    case assemble as of 
        Left err -> 
            putStrLn $ "Error: " ++ err
        Right s  -> 
            putStrLn $ f s

main :: IO ()
main = do
    args <- getArgs
    case listToMaybe args of
        Nothing               -> doAction ASM.Pretty.asmBytesOnly (assembly mainX86)
        Just "dump_bytes"     -> doAction ASM.Pretty.asmBytesOnly (assembly mainX86)
        Just "doc"            -> doAction ASM.Pretty.asmDocs      (assembly mainX86)

        -- Related to the testing of parsers
        Just "test_par"       -> doAction ASM.Pretty.asmBytesOnly (assembly mainTP)
        Just "test_par_doc"   -> doAction ASM.Pretty.asmDocs      (assembly mainTP)
        Just "test_par_stdin" -> mapM putStr parserTestSuiteStdin >> return ()

        -- X86 opcode generation tests
        Just "test_x86"       -> doAction ASM.Pretty.asmBytesOnly x86TestSuiteASM
        Just "test_x86_n"     -> putStr   $ x86TestSuiteNASM
        Just x                -> putStrLn $ "Unknown option \"" ++ x ++ "\"" ++
                                 " of (dump_bytes, doc, test_x86, test_x86_n)"

