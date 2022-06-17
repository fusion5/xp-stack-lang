module ASM.Pretty where

import ASM.Types
import ASM.ASM
import qualified Data.Sequence as S
import Data.Foldable (toList)
import qualified Data.ByteString.Lazy as BS
import Numeric (showHex)
import Data.List (intercalate, transpose, unfoldr)
import Data.Word
import Text.Printf


legend :: String
legend =
       "Legend:\n"
    ++ ".label means a label definition\n"
    ++ "{label} means a reference to a label (absolute)\n"
--    ++ "{Rel8/32 label} means a relative reference to a label (offset)\n"
    ++ "{label1 - label2} means a delta of label addresses\n"

-- Produces a sequence of ByteStrings that we must check against the expected
-- ByteStrings
asmTestRun :: (PrintfArg t_addr) => ASMState t_addr -> [BS.ByteString]
asmTestRun state =
    toList $ map asmTestItem $ toList $ contents state
  where
    asmTestItem :: (PrintfArg t_addr) => ASMItem t_addr -> BS.ByteString
    asmTestItem (ASMOpcode fileAddr virtAddr bytes opcode) =
        BS.concat $ map asmOpcodeBytes $ toList bytes
    asmTestItem _ = BS.empty
    asmOpcodeBytes (BytesLiteral bs) = bs
    asmOpcodeBytes _ = BS.empty

asmHex :: (PrintfArg t_addr) => ASMState t_addr -> String
asmHex state =
    intercalate "\n" $ map asmHexItem $ toList $ contents state
  where
    asmHexItem :: (PrintfArg t_addr) => ASMItem t_addr -> String
    asmHexItem (ASMOpcode fileAddr virtAddr bytes opcode) =
        printf "%08X %08X %s (%s)" fileAddr virtAddr (concat $ map asmHexBytes $ toList bytes) opcode
    asmHexItem (ASMBytes fileAddr virtAddr bytes) =
        printf "%08X %08X %s" fileAddr virtAddr (concat $ map asmHexBytes $ toList bytes)
    asmHexItem (ASMLabel fileAddr virtAddr label) =
        printf "                  .%s:" label
    asmHexItem (ASMComment comment) = (take 16 (repeat ' ')) ++ "# " ++ comment

chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

asmHexBytes :: ASMBytes t_addr -> String
asmHexBytes (BytesLiteral bs) =
    if BS.length bs > 0 then
        intercalate "\n" (first_chunk:other_chunks)
    else
        ""
    where
        first_chunk  = head chnks
        other_chunks = map (\x -> (take 18 (repeat ' ')) ++ x) (tail chnks)
        chnks = chunks 51 (dumpByteString bs)
asmHexBytes (BytesLabelRef _ _ _ s) = s ++ " "
asmHexBytes (BytesLabelRefOffset _ _ _ _ _ s) = s ++ " "
asmHexBytes (BytesLabelDiff ref _ _ s1 s2) = s2 ++ " - " ++ s1 ++ " (" ++ show ref ++ ")"


hex :: Int -> Char
hex 0  = '0'
hex 1  = '1'
hex 2  = '2'
hex 3  = '3'
hex 4  = '4'
hex 5  = '5'
hex 6  = '6'
hex 7  = '7'
hex 8  = '8'
hex 9  = '9'
hex 10 = 'A'
hex 11 = 'B'
hex 12 = 'C'
hex 13 = 'D'
hex 14 = 'E'
hex 15 = 'F'
hex _  = ' '

{-# INLINE hexBytes #-}
hexBytes :: Word8 -> (Char, Char)
hexBytes w = (hex h, hex l) where (h,l) = (fromIntegral w) `divMod` 16

-- | Dump one byte into a 2 hexadecimal characters.
hexString :: Word8 -> String
hexString i = [h,l] where (h,l) = hexBytes i

-- | Dump a list of word8 into a raw string of hex value
dumpRaw :: [Word8] -> String
dumpRaw ws = intercalate " " $ map hexString ws

dumpByteString :: BS.ByteString -> String
dumpByteString = dumpRaw . BS.unpack

