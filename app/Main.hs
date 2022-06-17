module Main where

import qualified ASM.ASM                  as ASM (compileASM)
import qualified ASM.Pretty               as ASM
import qualified ASM.Types                as ASM
import qualified Data.ByteString.Lazy     as BS
import qualified Data.Maybe               as Maybe (listToMaybe)
import qualified Runtime.Runtime          as Runtime
import qualified Runtime.X64.Windows      as X64Win
import qualified System.Environment       as Env
import qualified X64.TestUtils            as TestUtils
import qualified X64.Types                as X64
import qualified X64.X64                  as X64
import qualified Data.Word                as Word
import qualified Runtime.X64.Windows.PE64 as PE64

baseAddressWindows, baseAddressLinux :: Word.Word64
baseAddressWindows =   0x400000
baseAddressLinux   = 0xC0000000

assembleWindowsPortableExecutable :: X64.X64 ()
assembleWindowsPortableExecutable = PE64.pe64Header
  baseAddressWindows
  (fromIntegral Runtime.pstackSizeBytes)
  (fromIntegral Runtime.dictBodySizeBytes)
  X64Win.mainBody

windowsAssembly :: ASM.ASM Word.Word64 ()
windowsAssembly = do
  X64.runX64 assembleWindowsPortableExecutable
  ASM.compileASM

main :: IO ()
main = do
  args <- Env.getArgs
  case Maybe.listToMaybe args of
    Just "bootstrap_x86_windows_doc" -> do
      case X64.runASM_for_X64 0x400000 windowsAssembly of
        Left err ->
          putStrLn $ "Error: " ++ err
        Right finalState -> do
          putStrLn $ ASM.asmHex finalState
    Just "x86_testplan_nasm" -> do
      putStr TestUtils.x64TestSuiteNASM
    Just "x86_testrun" -> do
      case X64.runASM_for_X64 0x400000 (TestUtils.x64TestSuiteASM) of
        Left err ->
          putStrLn $ "Test ASM Error: " ++ err
        Right finalState -> do
          putStrLn $ unlines $ TestUtils.x64TestResults (ASM.asmTestRun finalState)
    Nothing -> do
      case X64.runASM_for_X64 0x400000 windowsAssembly of
        Left err ->
          putStrLn $ "Error: " ++ err
        Right finalState -> do
          putStrLn "Writing to main.exe..."
          BS.writeFile "main.exe" $ ASM.resolved_asm finalState
    _ -> putStrLn "Unrecognized cli arguments..."
