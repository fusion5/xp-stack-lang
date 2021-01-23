{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}

module ASM.ASM where

{-
import Data.Word
import Data.Char (ord, chr)
-- import Data.Map as M
import Data.Binary.Put
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.ByteString.Lazy.Char8 (ByteString, unpack, split)
import Data.Int
import Data.List (intercalate)

-}
import Debug.Trace 

import ASM.Datatypes

import Data.Sequence as S
import Data.ByteString.Lazy.Char8 (pack, singleton) 

import Control.Monad.Except
import Control.Monad.Trans.State
import Data.Binary.Put -- (putWord64le, putWord64be, putWord32le, putWord32be, Put, runPut)

import Data.Foldable as F
import Data.ByteString.Lazy as BS
import qualified Data.Map as M

import Data.Word
import Data.Int

{-
import Data.Sequence as S
-}

size :: Size -> Integer
size Bytes1 = 1
size Bytes2 = 2
size Bytes4 = 4
size Bytes8 = 8

lenBytes :: ASMBytes t_addr -> Integer
lenBytes (BytesLiteral bs) = fromIntegral $ BS.length bs
lenBytes (BytesLabelRef _ sz _ _) = size sz
lenBytes (BytesLabelRefOffset _ _ _ sz _ _) = size sz
lenBytes (BytesLabelDiff _ sz _ _ _) = size sz

lenItem :: ASMItem t_addr -> Integer
lenItem (ASMOpcode _ _ bsSeq _) = F.foldr (\x y -> lenBytes x + y) 0 bsSeq
lenItem (ASMComment _) = 0
lenItem (ASMLabel _ _ _) = 0
lenItem (ASMBytes _ _ bsSeq) = F.foldr (\x y -> lenBytes x + y) 0 bsSeq

append :: (Bounded t_addr, Integral t_addr)
       => ASMItem  t_addr
       -> ASMState t_addr
       -> ASMState t_addr 
append item state 
    | iMin <= new_file_addr && new_file_addr <= iMax &&
      iMin <= new_rva && new_rva <= iMax =
        state { contents    = contents state |> item
              , file_addr = fromIntegral $ new_file_addr
              , rva       = fromIntegral $ new_rva
              }
    where
        iMin = fromIntegral (z + minBound)
        iMax = fromIntegral (z + maxBound)
        z = zero state -- Just a hack to ensure type casting of t_addr
        l = lenItem item
        new_file_addr :: Integer = fromIntegral (file_addr state) + l
        new_rva       :: Integer = fromIntegral (rva       state) + l
append _ _ = error "Append out of bounds"

instance (Bounded t_addr, Integral t_addr) => Commentable (ASM t_addr) where
    comment = modify . ASM.ASM.append . ASMComment 

instance (Bounded t_addr, Integral t_addr) => Labelable (ASM t_addr) where
    label l = do
        -- Add the label to the map
        modify (\state -> state {
                    labels = M.insert l (file_addr state, rva state) (labels state)
               })
        -- Append the label (which increments the offsets)
        state <- get
        modify $ ASM.ASM.append $ ASMLabel (file_addr state) (rva state) l
        
-- Emit a specific string in ASM as bytes
stringASM :: (Bounded t_addr, Integral t_addr) => String -> ASM t_addr ()
stringASM string = do
    state <- get
    modify $ ASM.ASM.append $ 
        ASMBytes
            (file_addr state) 
            (rva state) 
            (S.singleton (BytesLiteral byteString))
  where 
    byteString = Data.ByteString.Lazy.Char8.pack string

-- Emit specific literal bytes in ASM
bytesASM :: (Bounded t_addr, Integral t_addr) => [Word8] -> ASM t_addr ()
bytesASM bytes = do
    state <- get
    modify $ ASM.ASM.append $ 
        ASMBytes 
            (file_addr state) 
            (rva state) 
            (S.singleton (BytesLiteral byteString))
  where
    byteString = BS.pack bytes

processASMBytes :: (Integral t_addr) => 
    M.Map String (t_addr, t_addr) -> ASMBytes t_addr -> ASM t_addr ByteString
processASMBytes labelMap (BytesLabelRef ref sz endi lbl) = 
  do
    a <- elookup lbl labelMap
    address <- addr ref a
    case convfSigned sz endi (toInteger address) of
        Just bs -> return bs
        Nothing -> throwError $ "Out of bounds label reference for \"" ++ lbl ++
                        "\" (requested a " ++ show (size sz) ++ "-byte representation)"
processASMBytes labelMap (BytesLabelRefOffset curr_file_addr curr_rva ref sz endi lbl) =
  do
    a <- elookup lbl labelMap
    labelAddr    <- addr ref a
    labelRefAddr <- addr ref (curr_file_addr, curr_rva)
    case convfSigned sz endi ((toInteger labelAddr - (toInteger labelRefAddr + size sz))  ) of
        Just bs -> return bs
        Nothing -> throwError $ "Out of bounds label offset to \"" ++ lbl ++
                        "\" (requested a " ++ show (size sz) ++ "-byte representation)"
processASMBytes labelMap (BytesLabelDiff ref sz endi lblBegin lblEnd) =
  do
    a1 <- elookup lblBegin labelMap
    a2 <- elookup lblEnd   labelMap
    resolvedA1 <- addr ref a1
    resolvedA2 <- addr ref a2
    let delta = toInteger resolvedA2 - toInteger resolvedA1
    -- case trace ("delta = " ++ show delta) $ convfUnsigned sz endi delta of
    case convfUnsigned sz endi delta of
        Just bs -> return bs
        Nothing -> throwError $ "Out of bounds label difference between " ++ lblBegin ++
            " and " ++ lblEnd ++ "!"
processASMBytes _ (BytesLiteral bs) = return bs

-- Resolve an address to the appropriate Relative Virtual, Image or Virtual type
addr :: (Integral t_addr) => Ref -> (t_addr, t_addr) -> ASM t_addr t_addr
addr RVA t = return $ snd t
addr IA  t = return $ fst t
addr VA  t = do imgBase <- imageBase
                return $ snd t + imgBase

imageBase :: ASM t_addr t_addr
imageBase = do
    s <- get
    return $ image_base s

-- testConv must be an array of all True values
-- DO NOT REMOVE THE TEST
testConv = [ convfSigned Bytes1 LE (-129) == Nothing
           , convfSigned Bytes1 LE (-128) == Just (BS.singleton 0x80)
           , convfSigned Bytes1 LE (-1)   == Just (BS.singleton 0xFF)
           , convfSigned Bytes1 LE 0      == Just (BS.singleton 0x00)
           , convfSigned Bytes1 LE 1      == Just (BS.singleton 0x01)
           , convfSigned Bytes1 LE (127)  == Just (BS.singleton 0x7F)
           , convfSigned Bytes1 LE (128)  == Nothing

           , convfSigned Bytes2 LE (-32769) == Nothing
           , convfSigned Bytes2 LE (-32768) == Just (BS.pack [0x00, 0x80])
           , convfSigned Bytes2 LE (-1)     == Just (BS.pack [0xFF, 0xFF])
           , convfSigned Bytes2 LE 0        == Just (BS.pack [0x00, 0x00])
           , convfSigned Bytes2 LE 1        == Just (BS.pack [0x01, 0x00])
           , convfSigned Bytes2 LE (32767)  == Just (BS.pack [0xFF, 0x7F])
           , convfSigned Bytes2 LE (32768)  == Nothing

           , convfUnsigned Bytes2 BE (-1)  == Nothing
           , convfUnsigned Bytes2 BE 0     == Just (BS.pack [0x80, 0x00])
           , convfUnsigned Bytes2 BE 1     == Just (BS.pack [0xFF, 0xFF])
           , convfUnsigned Bytes2 BE 65535 == Just (BS.pack [0x7F, 0xFF])
           , convfUnsigned Bytes2 BE 65536 == Nothing
           ]

-- Two's complement is assumed
-- The functions perform Integer conversion to a Word datatype with bound checking.
-- The result is a ByteString of an appropriate size.
convfSigned :: Size -> Endianness -> Integer -> Maybe ByteString
convfSigned Bytes1 _  x | -2^7  <= x && x <= (2^7  - 1) = Just $ bytes putWord8    (fromIntegral x)
convfSigned Bytes2 LE x | -2^15 <= x && x <= (2^15 - 1) = Just $ bytes putWord16le (fromIntegral x)
convfSigned Bytes2 BE x | -2^15 <= x && x <= (2^15 - 1) = Just $ bytes putWord16be (fromIntegral x)
convfSigned Bytes4 LE x | -2^31 <= x && x <= (2^31 - 1) = Just $ bytes putWord32le (fromIntegral x)
convfSigned Bytes4 BE x | -2^31 <= x && x <= (2^31 - 1) = Just $ bytes putWord32be (fromIntegral x)
convfSigned Bytes8 LE x | -2^63 <= x && x <= (2^63 - 1) = Just $ bytes putWord64le (fromIntegral x)
convfSigned Bytes8 BE x | -2^63 <= x && x <= (2^63 - 1) = Just $ bytes putWord64be (fromIntegral x)
convfSigned _ _ _ = Nothing

convfUnsigned :: Size -> Endianness -> Integer -> Maybe ByteString
convfUnsigned Bytes1 _  x | 0 <= x && x <= (2^8  - 1) = Just $ bytes putWord8    (fromIntegral x)
convfUnsigned Bytes2 LE x | 0 <= x && x <= (2^16 - 1) = Just $ bytes putWord16le (fromIntegral x)
convfUnsigned Bytes2 BE x | 0 <= x && x <= (2^16 - 1) = Just $ bytes putWord16be (fromIntegral x)
convfUnsigned Bytes4 LE x | 0 <= x && x <= (2^32 - 1) = Just $ bytes putWord32le (fromIntegral x)
convfUnsigned Bytes4 BE x | 0 <= x && x <= (2^32 - 1) = Just $ bytes putWord32be (fromIntegral x)
convfUnsigned Bytes8 LE x | 0 <= x && x <= (2^64 - 1) = Just $ bytes putWord64le (fromIntegral x)
convfUnsigned Bytes8 BE x | 0 <= x && x <= (2^64 - 1) = Just $ bytes putWord64be (fromIntegral x)
convfUnsigned _ _ _ = Nothing

bytes :: Integral a => (a -> Put) -> a -> BS.ByteString
bytes putFun n = runPut $ putFun n

elookup key map = 
    case M.lookup key map of
        Nothing -> throwError $ "Label reference not found: " ++ key
        Just x  -> return x

processASMItem :: (Integral t_addr) =>
    M.Map String (t_addr, t_addr) -> ASMItem t_addr -> ASM t_addr ByteString
processASMItem labelMap (ASMOpcode fa va bs s) = do
    bsSeq <- mapM (processASMBytes labelMap) bs
    return $ F.foldr BS.append BS.empty bsSeq
processASMItem labelMap (ASMBytes fa va bs) = do
    bsSeq <- mapM (processASMBytes labelMap) bs
    return $ F.foldr BS.append BS.empty bsSeq
processASMItem _ _ = return BS.empty

compileASM :: (Integral t_addr) => ASM t_addr () -- ByteString
compileASM = do
    s <- get
    -- TODO: would foldM be faster here?
    -- Converty all ASMItems in the sequence to ByteStrings (map):
    bsSeq <- mapM (processASMItem (labels s)) (contents s)
    modify (\state -> state { resolved_asm = F.foldr BS.append BS.empty bsSeq })
    -- Concatenate all ByteStrings in the Sequence (reduce):
    -- return $ F.foldr BS.append BS.empty bsSeq 

addVirtualBytes :: (Integral t_addr) => t_addr -> ASM t_addr ()
addVirtualBytes n =
    modify (\state -> state { rva = rva state + n})

alignRVA :: (Integral t_addr) => t_addr -> ASM t_addr ()
alignRVA alignment = 
    modify (\state -> state { rva = align (rva state) alignment })

-- Emit as many zero-bytes as needed to reach the desired file alignment
alignImage :: (Bounded t_addr, Integral t_addr) => t_addr -> ASM t_addr ()
alignImage alignment = do
    s <- get
    let have_addr = file_addr s
    let want_addr = align have_addr alignment 
    let n_zeroes  = fromIntegral (want_addr - have_addr)
    bytesASM $ Prelude.take n_zeroes $ Prelude.repeat 0

align n alignment = alignment * ((n + alignment - 1) `div` alignment)

testAlign =  
        [ (align 0  1) == 0
        , (align 10 1) == 10
        , (align 0  5) == 0
        , (align 1  5) == 5
        , (align 5  5) == 5
        , (align 6  5) == 10
        , (align 10 5) == 10
        , (align 10 6) == 12
        , (align 308 512) == 512
        ]

freshLabelWithPrefix :: String -> ASM t_addr String
freshLabelWithPrefix prefix = do
    s <- get
    let i = uid s
    modify new
    return $ prefix ++ show i
    where new s = s { uid = uid s + 1 }
