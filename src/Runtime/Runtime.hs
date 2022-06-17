{-# LANGUAGE MultiParamTypeClasses #-}

module Runtime.Runtime where

{- |In the Runtime module belongs all code that generates
   platform-dependent X86 code. It interfaces with the OS
   (Windows/Linux) or with the machine directly.
-}

{-
class (SWPlatform a, HWPlatform b) => Runtime a b where
  getSW :: a
  getHW :: b
  rdW8  :: asm ()
  wrW8  :: asm ()
-}

pstackSizeBytes   = 0x10000
dictBodySizeBytes = 0x10000
