{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lang.Datatypes where

import Lang.Types
import Data.Map as M
import X86.Datatypes
import ASM.Datatypes
import Data.Word

import Control.Monad.Trans.State

type Lang a = StateT LangState (X86_64) a

-- This might not be needed in the end, because types will
-- not be considered at compile time...
data LangState = LangState
    { lang_tyenv :: M.Map String Type
    }

type SeqParam = (String, Type)

data SeqInstr =
    SeqDefLit64 Word64
  | SeqDefTerm String

lit :: Word64 -> SeqInstr
lit = SeqDefLit64

run :: String -> SeqInstr
run = SeqDefTerm

envAddType :: String 
           -> Type 
           -> Lang ()
envAddType ident ty = do
    modify new 
  where
    new ls = ls { lang_tyenv = M.insert ident ty (lang_tyenv ls) }

x86 :: X86_64 a -> StateT LangState (X86_64) a
x86 action@(X86_64 _) = StateT f
    where f s = do k <- action
                   return (k, s)

x86asm :: ASM () -> Lang ()
x86asm = x86 . asm

initLangState = LangState { lang_tyenv = M.empty
                          }


