module Lang.Lang where

-- Language-specific functionality. This module sits on top of the
-- X86 monad.

import Control.Monad.Trans.State

import X86.Datatypes
import X86.X86
import ASM.ASM
import ASM.Datatypes

import Lang.Datatypes
import Lang.Types
import Lang.Linux

import Data.Int
import Data.Word

docLang = x86 . docX86 . ("Lang: " ++)

defineBaseFuns = do
    definePDrop

    defineCMP "EQ"  je
    defineCMP "LT"  jl
    defineCMP "LTE" jle
    defineCMP "GT"  jg
    defineCMP "GTE" jge

    defineBinop "PLUS"  add
    defineBinop "MINUS" sub
    defineBinop "TIMES" undefined
    defineBinop "AND"   and_

    defineShiftLeft
    defineShiftRight

    defineRdChrLinux
    defineWrChrLinux
    defineLit 

    defineExit

    defineDictBase
    defineEval
    testDefinition 

data Dict = 
    Entry (Maybe Word64) -- Pointer to the rest of the dictionary, if any.
          Int            -- Function name length
          String         -- Function name
          Word64         -- The address of the body of the function.

-- Bootstrap a kind of Forth interpreter dictionary.
-- The dictionary is a linked list of definitions. A definition consists
-- of a (k, prev) := body address
-- When interpreting an expression, we use the dictionary entries to find
-- the definitions of terms in the expression.
defineDictBase = do
    x86 $ asm $ do
        k <- appendDefinition Nothing  "PDROP"
        k <- appendDefinition (Just k) "EQ"
        k <- appendDefinition (Just k) "LT"
        k <- appendDefinition (Just k) "LTE"
        k <- appendDefinition (Just k) "GT"
        k <- appendDefinition (Just k) "GTE"
        k <- appendDefinition (Just k) "PLUS"
        k <- appendDefinition (Just k) "MINUS"
        k <- appendDefinition (Just k) "TIMES"
        k <- appendDefinition (Just k) "AND"
        k <- appendDefinition (Just k) "LIT"
        k <- appendDefinition (Just k) "EXIT"
        return ()
        
-- This is a sort of struct of C...
appendDefinition :: Maybe String -> String -> ASM String
appendDefinition prevLabel bodyLabel = do
    documentation $ "Dictionary definition of " ++ bodyLabel
    -- x <- freshLabelWithPrefix $ "DICT_ENTRY_" ++ bodyLabel ++ "_"
    let x = "DICT_ENTRY_" ++ bodyLabel
    -- Previous entry pointer
    setLabel x
    case prevLabel of
        Just l  -> emitLabelRef64 l
        Nothing -> bemit $ take 8 $ repeat (SWord8 0)
    bemit [SWord8 0] -- 0 means it's an ASM-based definition
                     -- 1 means it's an interpreter-based definition.
    -- TODO: throw error if bodyLabel exceeds 255 chars!
    bemit [SWord8 $ fromIntegral $ length bodyLabel]
    emitString bodyLabel
    emitLabelRef64 bodyLabel
    bflush
    return x

-- Takes 1 parameter and adds input from stdin
-- on the parameters stack as well as the result.
-- http://man7.org/linux/man-pages/man2/read.2.html
-- Type : -> :w64:w64
-- Func : -> :read_char:success
defineRdChrLinux :: Lang ()
defineRdChrLinux = defFunBasic funName ty body
  where 
    funName = "READ_CHAR"
    ty      = TyFunc "READ_CHAR" TyEmpty (TyProd baseWord TyWord)
    body = do
        -- push $ I32 0x00 -- Allocate 8 bytes on the call stack to read on
        ppush $ I8 0x00 -- Allocate 8bytes on the param stack

        x86 $ mov  rdx $ I64 0x01 -- Read 1 char
        x86 $ xor  rbx rbx        -- file descriptor: 0 <-> Read from stdin
        x86 $ mov  rax $ I64 $ fromIntegral linux_sys_read
        x86 $ mov  rcx rsi        -- Write to the top of the param stack, 1 byte...
        x86 $ int

        -- After the call, rax has the number of bytes that were read.
        -- Convert those bytes from the call stack buffer to the parameter
        -- stack, if any.
        x86 $ cmp rax $ I32 0
        x86 $ je "READ_CHARS_NOTHING_READ"

        -- One char read, in rax.
        -- TODO: Assert rax == 1 (This isn't possible for the time being)

        x86 $ xor rax rax
        ppopW8 al -- Take from stack the char we've read

        ppush rax     -- The char we've read
        ppush $ I64 1 -- Success
        returnFunc funName

        x86 $ asm $ setLabel "READ_CHARS_NOTHING_READ"
        ppush $ I64 0 -- Placeholder
        ppush $ I64 0 -- Fail
        returnFunc funName

-- Type :w64       -> :
-- Func :exit_code -> :
-- Exits the program with a specified exit code.
defineExit :: Lang ()
defineExit = defFunBasic "EXIT" ty body
    where
        ty   = TyFunc "EXIT" baseWord TyEmpty
        body = do
            ppop rbx
            x86 $ mov rax $ I64 1
            x86 $ int

-- http://man7.org/linux/man-pages/man2/write.2.html
-- Type :w64 -> :int
-- Func :chr -> :success
-- Side effect, prints buf on stdout.
defineWrChrLinux :: Lang ()
defineWrChrLinux = defFunBasic "WRITE_CHAR" ty body
    where 
        ty   = TyFunc "WRITE_CHAR" baseWord baseWord
        body = do
            x86 $ mov rdx $ I64 1 -- How many bytes to write?
            x86 $ mov rax $ I64 $ fromIntegral linux_sys_write
            x86 $ mov rbx $ I64 $ fromIntegral linux_stdout 
            -- read chars from the top of the params stack, i.e. from our buffer.
            x86 $ mov rcx rsi
            x86 $ int
            pdrop 1   -- Drop the top of the stack which was the char
                      -- we've just printed.
            ppush rax -- The putchar result, how many bytes we've written

-- Type:      :w64:w64 -> :w64
-- Operation: :a  :b   -> :a<<b
defineShiftLeft = defFunBasic fn ty body where
    fn   = "SHIFT_LEFT"
    ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
    body = do 
        ppop rcx -- shift
        ppop rax -- number
        x86 $ sal rax cl
        ppush rax

-- Type:      :w64:w64 -> :w64
-- Operation: :a  :b   -> :a<<b
defineShiftRight = defFunBasic fn ty body where
    fn   = "SHIFT_RIGHT"
    ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
    body = do 
        ppop rcx -- shift
        ppop rax -- number
        x86 $ sar rax cl
        ppush rax

-- Type:      : -> :w64
-- Operation: : -> :literal
defineLit = defFunBasic fn ty body where
    fn = "LIT"
    ty = TyFunc fn TyEmpty baseWord
    body = do
        docLang "Look up the call stack to find out the position from which"
        docLang "LIT was called:"
        x86 $ ctop rax
        docLang "Place on rbx the word found after the parent LIT reference:"
        x86 $ mov rbx (derefOffset rax 0)
        docLang "Push the value from rbx (the literal) onto the stack."
        ppush rbx
        docLang "Manipulate the call stack so that the RET that follows"
        docLang "skips the literal we have just read (which by no means"
        docLang "should be returned to and executed!)"
        x86 $ add rsp $ I32 8

-- A sequence definition attempt which we want to evaluate.
testDefinition :: Lang ()
testDefinition = do
    x86 $ asm $ setLabel "TEST_SEQUENCE"
    x86 $ asm $ emitLabelRef64 "DICT_ENTRY_LIT"
    x86 $ imm64 $ 42
    x86 $ asm $ bflush
    x86 $ asm $ emitLabelRef64 "DICT_ENTRY_LIT"
    x86 $ imm64 $ 42
    x86 $ asm $ bflush
    x86 $ asm $ emitLabelRef64 "DICT_ENTRY_PLUS"
    x86 $ asm $ emitLabelRef64 "DICT_ENTRY_EXIT"

defineEval :: Lang ()
defineEval = do
    x86 $ asm $ setLabel "EVAL"
    -- TODO: Define some sort of struct type/GADT and use that to access
    -- the dictionary entry fields maybe, to handle more complexity?
    -- RAX is a pointer to the sequence to be evaluated.
    -- The first entry in the sequence is a dictionary entry.
    -- If the value at address RAX is 0, then we have an ASM-based entry. 
    docLang "RAX holds the sequence of word definition locations to evaluate."
    docLang "Move the entry type (at offset rax + 8) to rbx"
    x86 $ mov rax (derefOffset rax 0)
    x86 $ mov rbx (derefOffset rax 8)
    docLang "If the entry type equals 0 then evaluate a base function..."
    x86 $ cmp rbx (I32 0x00)
    x86 $ jeNear "EVAL_BASE_FUNCTION"
    x86 $ asm $ setLabel "EVAL_BASE_FUNCTION"
    docLang "The length of the dictionary entry name is at offset 16"
    x86 $ mov rbx (derefOffset rax 16) 
    x86 $ add rbx (I32 16)
    docLang "Now rbx holds a reference to the address we need to call."
    x86 $ call rbx
    x86 $ ret 

-- Type:      :w64:w64 -> w64
-- Operation: :a  :b   -> a (x) b
-- defineBinop :: String -> X86_64()
-- FIXME: Think about / introduce IMUL/IPLUS etc for signed/unsigned values!
-- We might need overflow checking also, i.e. return success or failure.
defineBinop fn op
    | fn == "PLUS" || fn == "MINUS" || fn == "AND" =
    defFunBasic fn ty body where
        ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
        body = do
            ppop rax
            ppop rbx
            x86 $ op rax rbx
            ppush rax -- Result
defineBinop fn _ 
    | fn == "TIMES" = do
    defFunBasic fn ty body where
        ty   = TyFunc fn (TyProd baseWord TyWord) baseWord
        body = do
            ppop rax
            ppop rbx
            x86 $ mul rbx  -- rax <- rax * rbx
            ppush rax -- Result


defineCMP :: String -> (String -> X86_64()) -> Lang ()
defineCMP funName jmpCmpFun = defFunBasic funName ty funBody
  where
    funBody = do
        ppop rax
        ppop rbx
        x86 $ cmp rax rbx -- test: rax (?) rbx
        x86 $ jmpCmpFun trueLabel -- if cmpFun holds, jump to trueLabel
        ppush (I64 0)
        x86 $ ret
        x86 $ asm $ setLabel trueLabel
        ppush (I64 1)
        x86 $ ret
    ty = TyFunc funName (TyProd baseWord TyWord) baseWord
    trueLabel = funName ++ "_true"

definePDrop :: Lang ()
definePDrop = defFunBasic "PDROP" ty body
    where body = pdrop 1
          ty   = TyFunc "PDROP" baseWord TyEmpty

-- Parameter stack drop
pdrop :: Int32 -> Lang ()
pdrop numW64s =
    x86 $ add rsi $ I32 $ fromIntegral $ numW64s * 8

-- Parameter stack pop
ppop :: Val -> Lang ()
ppop dst64@(R64 _) = do
    x86 $ mov dst64 $ derefOffset rsi 0
    x86 $ add rsi $ I32 8

ppopW8 :: Val -> Lang ()
ppopW8 dst@(R8 reg) = do
    x86 $ mov dst $ derefOffset rsi 0
    x86 $ add rsi $ I32 1

cpeerW8 :: Int32 -> Val -> X86_64 ()
cpeerW8 numW8s dst =
    mov dst $ derefOffset rsp (fromIntegral numW8s)

cpeer :: Int32 -> Val -> X86_64 ()
cpeer numW64s dst = 
    mov dst $ derefOffset rsp (fromIntegral $ numW64s * 8)

ctop :: Val -> X86_64 ()
ctop = cpeer 0

-- Parameter push on the parameter stack (pstack), a different 
-- stack from the call stack (for which we use simply push).
-- For the parameter stack position, we reserve the RSI register.
-- Programs should avoid this register!
-- void -> 8 bytes space of any type
-- TODO: Consider the benefits of only supporting w64 on pstack and have 
-- another stack for w8?
-- FIXME: Rather than RSI use RBP which isn't used anyway.
ppush :: Val -> Lang ()
ppush v | supported v = do
    docLang $ "Parameter stack push from register " ++ show v
    x86 $ sub rsi $ I32 $ sz v
    x86 $ mov (derefOffset rsi 0) v
    where sz (I64 _)    = 8
          sz (R64 _)    = 8
          sz (RR64 _ _) = 8
          sz (I8 _)     = 1
          sz (R8 _)     = 1
          supported (I8 _) = True
          supported (R8 _) = True
          supported (I64 _) = True
          supported (R64 _) = True
          supported _ = False
ppush v = error $ "Don't know how to ppush " ++ show v

-- A function made up of assembly which has a type.
-- This is used to define the built-in (basic) functions
-- No type checking is performed on the body!
defFunBasic :: String
            -> Type
            -> Lang ()
            -> Lang ()
defFunBasic funName funTy funBody = do
    envAddType funName funTy
    x86 $ asm $ setLabel funName
    funBody
    x86 $ asm $ setLabel (funName ++ "_return")
    x86 $ ret

-- A helper function to jump to the return point of the function.
returnFunc funName = do
    x86 $ jmpLabel $ funName ++ "_return"

runLang :: Lang a 
        -> X86_64 (a, LangState)
runLang stm = runStateT stm initLangState
