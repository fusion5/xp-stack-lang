{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
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
import qualified Data.Bits as B

-- String hash parameters
fnvOffsetBasis = 0xCBF29CE484222325
fnvPrime       = 0x100000001B3

fnvFold :: Bool -> Word8 -> Word64 -> Word64
fnvFold False x h = (fnvPrime * h) `B.xor` fromIntegral x
fnvFold True  x h = fnvPrime * (h  `B.xor` fromIntegral x)

-- A helper function to compute a fnv-1 hash
fnv1 :: [Word8] -> Word64
fnv1 = foldr (fnvFold False) fnvOffsetBasis

instance Documentation (Lang ()) where
    doc = x86 . doc

docLang :: String -> Lang ()
docLang = doc

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

    defineRdTermLinux

    defineHashTerm

    defineLit 
    definePush1 -- Dummy test function that just pushes constant 1

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
    x86 $ do
        k <- appendASMDefinition Nothing  "PDROP"
        k <- appendASMDefinition (Just k) "EQ"
        k <- appendASMDefinition (Just k) "LT"
        k <- appendASMDefinition (Just k) "LTE"
        k <- appendASMDefinition (Just k) "GT"
        k <- appendASMDefinition (Just k) "GTE"
        k <- appendASMDefinition (Just k) "PLUS"
        k <- appendASMDefinition (Just k) "MINUS"
        k <- appendASMDefinition (Just k) "TIMES"
        k <- appendASMDefinition (Just k) "AND"
        k <- appendASMDefinition (Just k) "LIT"
        k <- appendASMDefinition (Just k) "EXIT"
        k <- appendASMDefinition (Just k) "PUSH1"
        k <- appendSEQDefinition (Just k) "PUSH3"
        return ()
        
appendSEQDefinition :: Maybe String -> String -> X86_64 String
appendSEQDefinition prevLabel bodyLabel = do
    docX86 $ "Dictionary entry for " ++ bodyLabel
    let x = "DICT_" ++ bodyLabel
    asm $ setLabel x
    case prevLabel of
        Just l  -> asm $ emitLabelRef64 l
        Nothing -> imm64 0
    asm $ bflush
    docX86 $ "Type:"
    imm64 1 -- Sequence-based definition (to be evaluated using EVAL)
    asm $ bflush
    docX86 $ "Seq address (will the seq reside on the stack also sometimes?):"
    asm $ emitLabelRef64 $ bodyLabel
    asm $ bflush
    -- TODO: throw error if bodyLabel exceeds 255 chars!
    docX86 $ "Label length:"
    imm64 $ fromIntegral $ length bodyLabel
    asm $ bflush
    docX86 $ "Dictionary entry label:"
    asm $ emitString bodyLabel
    asm $ bflush
    return x

-- This is a sort of struct of C...
appendASMDefinition :: Maybe String -> String -> X86_64 String
appendASMDefinition prevLabel bodyLabel = do
    docX86 $ "Dictionary entry for " ++ bodyLabel
    -- x <- freshLabelWithPrefix $ "DICT_ENTRY_" ++ bodyLabel ++ "_"
    let x = "DICT_" ++ bodyLabel
    -- Previous entry pointer
    asm $ setLabel x
    case prevLabel of
        Just l  -> asm $ emitLabelRef64 l
        Nothing -> imm64 0
    asm $ bflush
    docX86 $ "Hash: TODO"
    -- TODO: Add the hash to assist in the term search!
    docX86 $ "Type:"
    imm64 0 -- ASM-based definition
    asm $ bflush
    docX86 $ "ASM code address:"
    asm $ emitLabelRef64 bodyLabel
    asm $ bflush
    -- TODO: throw error if bodyLabel exceeds 255 chars!
    docX86 $ "Label length:"
    imm64 $ fromIntegral $ length bodyLabel
    asm $ bflush
    docX86 $ "Dictionary entry label:"
    asm $ emitString bodyLabel
    asm $ bflush
    return x

defineHashTerm :: Lang ()
defineHashTerm = defFunBasic funName ty body
  where
    -- Improve: Is HASH_TERM expressible as a sequence of more basic words?
    funName = "HASH_TERM"
    ty      = undefined -- Unexpressible atm.
    body    = do
        docLang "Given a word on the stack, compute its hash. This helps to"
        docLang "compare words for equality when searching the dictionary."
        docLang "Trying out the (simple) FNV Hash function from Wikipedia"
        docLang "RBX holds the length to iterate."
        docLang "E.g. 'cba' has hash 15626587013303479755"
        ppop rbx
        docLang "RAX holds the hash."
        x86 $ mov rax $ I64 fnvOffsetBasis

        x86 $ asm $ setLabel "HASH_TERM_WHILE"
        do 
            x86 $ cmp rbx $ I32 0
            x86 $ jeNear "HASH_TERM_BREAK"
            x86 $ mov rcx (I64 fnvPrime)
            x86 $ mul rcx
            x86 $ xor rcx rcx -- Zeroing rcx is necessary
            ppopW8 cl
            x86 $ xor rax rcx
            x86 $ dec rbx
            x86 $ jmpLabel "HASH_TERM_WHILE"
        x86 $ asm $ setLabel "HASH_TERM_BREAK"

        ppush rax
        x86 $ ret


defineRdTermLinux :: Lang ()
defineRdTermLinux = defFunBasic funName ty body
  where
    funName = "READ_TERM"
    ty      = undefined -- Unexpressible atm.
    body    = do
        docLang "Reads characters until a space or a new line char is read."
        docLang "It pushes on the stack an array of w8 then it pushes a"
        docLang "w64 indicating how many chars have been read,"
        docLang "followed by a w64 indicating success or failure."

        docLang "r15 counts the chars that are successfully read."
        x86 $ xor r15 r15

        docLang "Prepare registers for the system call:"
        docLang "We'll always read 1 char:"
        x86 $ mov rdx $ I64 0x01 
        docLang "Always use file descriptor 0 (stdin):"
        x86 $ xor rbx rbx

        do  x86 $ asm $ setLabel "READ_TERM_WHILE"
            docLang "Use the linux_sys_read system call:"
            x86 $ mov rax $ I64 $ fromIntegral linux_sys_read

            docLang "Allocate 8 bytes on the param stack:"
            ppush $ I8 0x00

            docLang "Where to write? To the top of the param stack:"
            x86 $ mov rcx rsi
            x86 $ int

            x86 $ cmp rax $ I32 0x00
            x86 $ je "READ_TERM_ERROR"

            docLang "If the char we just read is a space or eol, break the loop:"
            x86 $ xor rax rax
            ptopW8 al
            x86 $ cmp rax $ I32 0x20
            x86 $ je "READ_TERM_BREAK"
            x86 $ cmp rax $ I32 0x0A
            x86 $ je "READ_TERM_BREAK"
            
            x86 $ inc r15

            docLang  "Repeat the operation"
            x86 $ jmpLabel "READ_TERM_WHILE"

        x86 $ asm $ setLabel "READ_TERM_BREAK"
        ppopW8 al
        ppush $ r15   -- Num of chars read
        ppush $ I64 1 -- Success
        x86 $ ret

        x86 $ asm $ setLabel "READ_TERM_ERROR"
        ppush $ r15   -- Num of chars read
        ppush $ I64 0 -- Error
        x86 $ ret

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
        docLang "Allocate 8 bytes on the param stack:"
        ppush $ I8 0x00

        x86 $ do 
            docX86 "Read 1 char:"
            mov  rdx $ I64 0x01 
            docX86 "File descriptor 0 <-> read from stdin:"
            xor  rbx rbx
            docX86 "Use the linux_sys_read system call:"
            mov  rax $ I64 $ fromIntegral linux_sys_read
            docX86 "Where to write? To the top of the param stack:"
            mov  rcx rsi
            int

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

definePush1 = defFunBasic fn ty body where
    fn = "PUSH1"
    ty = TyFunc fn TyEmpty baseWord
    body = do
        ppush (I64 1)

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
testDefinition = x86 $ do
    asm $ setLabel "TEST"
    asm $ emitLabelRef64 "DICT_PUSH3"
    asm $ bflush
    asm $ emitLabelRef64 "DICT_PLUS"
    asm $ bflush
    asm $ emitLabelRef64 "DICT_PLUS"
    asm $ bflush
    asm $ emitLabelRef64 "DICT_EXIT"
    asm $ bflush
    imm64 0 -- Marks the sequence end
    asm $ bflush

    asm $ setLabel "PUSH3"
    asm $ emitLabelRef64 "DICT_PUSH1"
    asm $ bflush
    asm $ emitLabelRef64 "DICT_PUSH1"
    asm $ bflush
    asm $ emitLabelRef64 "DICT_PUSH1"
    asm $ bflush
    imm64 0 -- Marks the sequence end
    asm $ bflush

defineEval :: Lang ()
defineEval = do
    x86 $ asm $ setLabel "EVAL"
    -- TODO: Define some sort of struct type/GADT and use that to access
    -- the dictionary entry fields maybe, to handle more complexity?
    -- RAX is a pointer to the sequence to be evaluated.
    -- The first entry in the sequence is a dictionary entry.
    -- If the value at address RAX is 0, then we have an ASM-based entry. 
    docLang "r8 holds the sequence of words (dict. pointers) to evaluate."
    docLang "We use r9 to iterate the words in the sequence."
    docLang "Initially r9 holds the memory location of the first word:"
    -- docLang "It's a pointer to a 'struct' of definitions with metadata."
    x86 $ mov r9 (derefOffset r8 0)
    x86 $ cmp r9 (I32 0x00)
    x86 $ jeNear "EVAL_STOP" -- Empty word reached - the sequence ends

    docLang "r9 + 0:  Previous dictionary entry (if any)"
    docLang "r9 + 8:  Type of dictionary entry (asm, words or typedef)"
    docLang "r9 + 16: Word definition body pointer"
    docLang "r9 + 24: Length of dictionary name (l)"
    docLang "r9 + 32: Name of dictionarly entry of length l" 

    docLang "Move the entry type (located at offset r9 + 8) to r10"
    x86 $ mov r10 (derefOffset r9 8)

    docLang "Pattern match on the type of dictionary entry:"
    x86 $ cmp r10 (I32 0x00)
    x86 $ jeNear "EVAL_ASM"
    x86 $ cmp r10 (I32 0x01)
    x86 $ jeNear "EVAL_WORDS"

    docLang "Type = 0: Evaluating assembly"
    docLang "Move the method body pointer to r10"
    docLang "The method body pointer is at r9+16"
    x86 $ do
        asm $ setLabel "EVAL_ASM"
        mov r10 (derefOffset r9 16)
        call r10 
        jmpLabel "EVAL_STEP_DONE"

    docLang "Type = 1: Evaluating a sequence of words"
    docLang "Recursively call EVAL on a sequence."
    docLang "The sequence to be called is at r9 + 16."
    docLang "Prepare for eval recursion: save the current r8 on the "
    docLang "call stack."
    x86 $ do 
        asm $ setLabel "EVAL_WORDS"
        push r8
        mov r8 (derefOffset r9 16)
        callLabel "EVAL" -- Recursion
        pop r8
        jmpLabel "EVAL_STEP_DONE"

    x86 $ asm $ setLabel "EVAL_STEP_DONE"

    -- TODO: Move on to the next word in the sequence we're evaluating.
    x86 $ add r8 (I32 8)
    x86 $ jmpLabel "EVAL" -- We've advanced to the next word, repeat the operation.

    x86 $ asm $ setLabel "EVAL_STOP"
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

-- Parameter stack drop, alters the parameter stack register rsi
pdrop :: Int32 -> Lang ()
pdrop numW64s =
    x86 $ add rsi $ I32 $ fromIntegral $ numW64s * 8

pdropW8 :: Int32 -> Lang ()
pdropW8 numW8s = 
    x86 $ add rsi $ I32 $ fromIntegral $ numW8s

-- Parameter stack pop
ppop :: Val -> Lang ()
ppop dst64@(R64 _) = do
    x86 $ mov dst64 $ derefOffset rsi 0
    x86 $ add rsi $ I32 8

ppeer :: Int32 -> Val -> Lang ()
ppeer numW64s dst =
    x86 $ mov dst $ derefOffset rsi (fromIntegral $ numW64s * 8)

ppeerW8 :: Int32 -> Val -> Lang ()
ppeerW8 numW8s dst@(R8 r)  =
    x86 $ mov dst $ derefOffset rsi (fromIntegral numW8s)
ppeerW8 _ _ = error "ppeerW8 requires an 8-bit register as parameter"

ptopW8 :: Val -> Lang ()
ptopW8 = ppeerW8 0

ppopW8 :: Val -> Lang ()
ppopW8 dst@(R8 reg) = x86 $ do
    mov dst $ derefOffset rsi 0
    add rsi $ I32 1

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
    docLang $ "ppush from register " ++ show v
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
ppush v = error $ "ppush doesn't support " ++ show v

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
