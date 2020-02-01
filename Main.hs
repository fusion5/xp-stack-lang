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
--
import Data.Word

import Lang.Lang
import Lang.Datatypes
import Lang.Types
import Lang.Debug

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
    -- x86 $ callLabel "READ_TERM"
    -- assertPtop 1 "READ_TERM should be successful"
    -- pdrop 1

    -- Test for input string 'abc'
    {-
    assertPtop 3 "Please input 3 chars"
    pdrop 1
    assertPtopW8 0x63 "Please input c third"
    pdropW8 1
    assertPtopW8 0x62 "Please input b second"
    pdropW8 1
    assertPtopW8 0x61 "Please input a first"
    -}

    -- x86 $ callLabel "HASH_TERM"

    -- assertPtop (fromIntegral $ fnv1 [0x61, 0x62, 0x63]) 
    --    "The hash should match the one computed in HS"

    pushInitialDictionary

    docLang "The 0 is needed because after it will follow the "
    docLang "address of the MAIN dictionary symbol. The purpose is "
    docLang "to build an entry point, and EVAL takes as input a sequence of "
    docLang "dictionary entry pointers ending in 0. This is the ending 0."
    ppush $ I32 0

    docLang "Dictionary search function:"
    let searchFun = "MAIN"
    mapM ppush $ map (I8 . ascii) searchFun
    ppush $ I32 $ fromIntegral $ length searchFun
    x86 $ callLabel "TERM_LOOK"

    assertPtop 1 "MAIN term definition not found"
    pdrop 1 -- TERM_LOOK success code

    docLang "We initialize r8 to be the current stack top, because "
    docLang "as mentioned before, the stack now holds a sequence that "
    docLang "is to be passed to the EVAL function (and EVAL takes r8 "
    docLang "as parameter)."
    x86 $ mov r8 rsi
    x86 $ callLabel "EVAL"

    x86 $ callLabel "EXIT"

    {-
    docLang "Consume the word on the stack in the dictionary and push it."

    docLang "TEST_SEQUENCE_1:"
    x86 $ mov r8 (L64 "TEST_SEQUENCE1") -- What to interpret
    x86 $ callLabel "EVAL"    -- Eval function
    -}
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
    elf64Header $ programHeader vaddr_offset $ do
        mainASM 
        documentation "The string table with all collected strings:"
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
        Nothing           -> doAction ASM.Pretty.asmBytesOnly assembly
        Just "dump_bytes" -> doAction ASM.Pretty.asmBytesOnly assembly
        Just "doc"        -> doAction ASM.Pretty.asmPretty    assembly
        Just "test_x86"   -> doAction ASM.Pretty.asmBytesOnly x86TestSuiteASM
        Just "test_x86_n" -> putStr   $ x86TestSuiteNASM
        Just x            -> putStrLn $ "Unknown option \"" ++ x ++ "\"" ++
                             " of (dump_bytes, doc, test_x86, test_x86_n)"

