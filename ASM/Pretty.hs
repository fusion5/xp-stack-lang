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

legend :: String 
legend = 
       "Legend:\n"
    ++ ".label means a label definition\n"
    ++ "{label} means a reference to a label (absolute)\n"
    ++ "{Rel8/32 label} means a relative reference to a label (offset)\n"
    ++ "{StrRef x} means a reference to a string in the string table\n"
    ++ "{Program Size} is the size of the whole program."

asmPretty :: ASMState -> String
asmPretty endASMState = 
    (intercalate "\n" $ map asmCodePretty $ toList $ asm_instr endASMState)
    ++ "\n"
    ++ legend
    ++ "\n"
    ++ "All labels:"
    ++ "\n"
    ++ (intercalate "\n" $ map ppLabelDef $ M.toList $ asm_lbls endASMState)

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

-- Print only the bytes... it gets used at building a binary file.
asmBytesOnly :: ASMState -> String
asmBytesOnly endASMState = 
    concat $ map asmBytesOnlyCode $ toList $ asm_instr endASMState

asmBytesOnlyCode :: ASMCode -> String
asmBytesOnlyCode (ASMEmit _ instrs) =
    concat $ map asmBytesOnlyEmit instrs
asmBytesOnlyCode _ = ""


asmBytesOnlyEmit :: ASMEmit String -> String
asmBytesOnlyEmit (SWord8 w8) = showWord8 w8 ++ " "
asmBytesOnlyEmit _ = ""
