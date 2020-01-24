{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment

import ASM.Datatypes        -- Common datatypes
import ASM.ASM
import ASM.Pretty       -- Print ASM with nice comments (documentation)
import ELFHeader        -- ELF header
-- import Linux         -- Linux constants
import X86.Datatypes    -- X86-specific assembly datatypes
import X86.X86          -- X86-specific assembly code
import X86.Tests        -- X86-specific test suite (with NASM commands)
-- import X86.Debug     -- X86-specific debug helper functions
-- import X86.Functions -- Functions
-- import X86.Types     -- Types for the functions
-- import X86.AST       -- Abstract syntax tree

import Lang.Lang
import Lang.Datatypes
import Lang.Types

import Data.Maybe (listToMaybe)
import Data.List (intercalate)

initParamStack :: X86_64 ()
initParamStack = do
    docX86 "Initialise the heap. brk(), the Linux system call that does this"
    docX86 "has id number 45. We will use the heap as a function parameter"
    docX86 "stack, i.e. it is used to pass parameters to functions."
    mov rax (I64 45)
    mov rbx (I64 0)
    int

    docX86 "rsi holds the heap address"
    mov rsi rax 

    docX86 "A second brk() call is needed."
    mov rax (I64 45) 
    docX86 "Set the heap start address as brk argument"
    mov rbx rsi
    docX86 "Allocate 10k bytes for the heap"
    add rbx (I32 10000)
    int
    -- FIXME: If the heap alloc didn't work then this 
    -- should throw a segmentation fault.
    docX86 "The parameter stack grows from large to smal, "
    docX86 "similar to the call stack"
    add rsi (I32 10000) 

mainLang = do
    docLang "Read from stdin the dict. entry that we should interpret."
    x86 $ callLabel "READ_LINE"

    docLang "TESTS:"
    x86 $ mov r8 (L64 "TEST") -- What to interpret
    x86 $ callLabel "EVAL" -- Eval function
    defineBaseFuns

mainX86 = do
    mov rbp rsp
    initParamStack
    -- Run the language standard function definitions in the Lang monad
    (a, finalLangState) <- runLang mainLang
    return a

mainASM = do
    runX86 mainX86

assembly = do
    elf64Header $ programHeader vaddr_offset mainASM 
    replaceProgSizeWithBytes
    replaceLabelsWithBytes vaddr_offset
    documentation "The string section with all collected strings:"
    emitStrings
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
        Nothing           -> doAction ASM.Pretty.asmBytesOnly assembly
        Just "dump_bytes" -> doAction ASM.Pretty.asmBytesOnly assembly
        Just "doc"        -> doAction ASM.Pretty.asmPretty    assembly
        Just "test_x86"   -> doAction ASM.Pretty.asmBytesOnly x86TestSuiteASM
        Just "test_x86_n" -> putStr   $ x86TestSuiteNASM
        Just x            -> putStrLn $ "Unknown option \"" ++ x ++ "\""


