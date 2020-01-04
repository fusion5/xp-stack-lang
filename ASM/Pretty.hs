module ASM.Pretty where

import ASM.Datatypes
import ASM.ASM
import Data.List (intercalate)
import Data.Foldable (toList)
import Numeric (showHex)
import Text.Printf
import Data.Char (toUpper)

legend :: String 
legend = 
       "Legend:\n"
    ++ ".label means a label definition\n"
    ++ "{label} means a reference to a label (offset or absolute)\n"
    ++ "{StrRef x} means a reference to a string in the string table\n"
    ++ "{Program Size} is the size of the whole program."

asmPretty :: ASMState -> String
asmPretty endASMState = 
    (intercalate "\n" $ map asmCodePretty $ toList $ asm_instr endASMState)
    ++ "\n"
    ++ "\n"
    ++ legend

asmCodePretty :: ASMCode -> String
asmCodePretty (ASMEmit offset instrs) = 
    printf "0x%08X %s" offset $ concat $ map asmEmitPretty instrs
    -- showHex offset "" ++ " : " ++ show instrs
asmCodePretty (ASMLabel offset label) = 
    ". " ++ label
asmCodePretty (ASMDoc offset doc) = 
    "# " ++ doc

asmEmitPretty (SWord8 w8) = showWord8 w8 ++ " "-- (showHex w8 " ")
asmEmitPretty (SProgLabel64 lt) = "{" ++ lt ++ "} " 
asmEmitPretty (SProgLabel32 lt) = "{" ++ lt ++ "} " 
asmEmitPretty (SRelOffsetToLabel32 _ lt) = "{" ++ lt ++ "} " 
asmEmitPretty (SRelOffsetToLabel8  _ lt) = "{" ++ lt ++ "} " 
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
