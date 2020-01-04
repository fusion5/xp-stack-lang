{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lang.Datatypes where

import Lang.Types
import Data.Map as M
import X86.Datatypes
import ASM.Datatypes

import Control.Monad.Trans.State

type Lang a = StateT LangState (X86_64) a

data LangState = LangState
    { lang_tyenv :: M.Map String Type
    }

envAddType :: String -> Type -> Lang ()
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


