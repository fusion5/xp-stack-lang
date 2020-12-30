module ASM.Pretty where

import ASM.Datatypes
import ASM.ASM
import Data.List (intercalate)
import Data.Foldable (toList)
import Numeric (showHex)
import Text.Printf
import Data.Char (toUpper)
import qualified Data.Map as M
import Data.Word
-- import Data.Text.Internal.Unsafe.Char (unsafeChr8)
import qualified Data.ByteString as BS

legend :: String 
legend = 
       "Legend:\n"
    ++ ".label means a label definition\n"
    ++ "{label} means a reference to a label (absolute)\n"
    ++ "{Rel8/32 label} means a relative reference to a label (offset)\n"
    ++ "{StrRef x} means a reference to a string in the string table\n"
    ++ "{Program Size} is the size of the whole program."

asmDocs :: ASMState -> String
asmDocs endASMState = 
    (intercalate "\n" $ map asmCodePretty $ toList $ asm_instr endASMState)
    ++ "\n"
    ++ "All labels:\n"
    ++ (intercalate "\n" $ map ppLabelDef $ M.toList $ asm_lbls endASMState)
    ++ "\n"
    ++ "All strings:\n"
    ++ (intercalate "\n" $ map ppLabelDef $ M.toList $ asm_strs endASMState)
    ++ "\n"
    ++ legend

ppLabelDef :: (String, Word64) -> String
ppLabelDef (x, y) = printf "0x%08X %s" y x

asmCodePretty :: ASMCode -> String
asmCodePretty (ASMEmit offset instrs) = 
    printf "0x%08X %s" offset (concat $ map asmEmitPretty instrs)
    -- printf "%s" $ concat $ map asmEmitPretty instrs
    -- showHex offset "" ++ " : " ++ show instrs
asmCodePretty (ASMLabel offset label) = 
    ". " ++ label
asmCodePretty (ASMDoc offset doc) = 
    "         # " ++ doc

asmEmitPretty (SWord8 w8) = showWord8 w8 ++ " "-- (showHex w8 " ")
asmEmitPretty (SProgLabel64 lt) = "{" ++ lt ++ "} " 
asmEmitPretty (SProgLabel32 lt) = "{" ++ lt ++ "} " 
asmEmitPretty (SRelOffsetToLabel32 currOff64 lt) = 
    "{Rel32 " ++ lt ++ "} " 
asmEmitPretty (SRelOffsetToLabel8 currOff64 lt) = "{Rel8 " ++ lt ++ "} " 
asmEmitPretty (SProgSize64) = "{Program Size} " 
asmEmitPretty (SStrRef64 s) = "{StrRef " ++ s ++ "} " 
asmEmitPretty (SLabelDiff32 l1 l2) = "{" ++ l2 ++ " - " ++ l1 ++ "} " 
asmEmitPretty (SLabelDiff16 l1 l2) = "{" ++ l2 ++ " - " ++ l1 ++ "} " 

-- Print only the bytes... it gets used at building a binary file.
asmHex :: ASMState -> String
asmHex endASMState = 
    concat $ map asmHexCode $ toList $ asm_instr endASMState

asmHexCode :: ASMCode -> String
asmHexCode (ASMEmit _ instrs) =
    concat $ map asmHexEmit instrs
asmHexCode _ = ""

asmHexEmit :: ASMEmit String -> String
asmHexEmit (SWord8 w8) = showWord8 w8 ++ " "
asmHexEmit _ = ""

asmBin :: ASMState -> BS.ByteString
asmBin endASMState = 
    BS.concat $ map asmBinCode $ toList $ asm_instr endASMState

asmBinCode :: ASMCode -> BS.ByteString
asmBinCode (ASMEmit _ instrs) =
    BS.concat $ map asmBinEmit instrs
asmBinCode _ = BS.empty

asmBinEmit :: ASMEmit String -> BS.ByteString
asmBinEmit (SWord8 w8) = BS.singleton w8
asmBinEmit _ = BS.empty

