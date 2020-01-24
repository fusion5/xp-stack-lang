{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module ASM.ASM where

import Control.Monad.Except
import Control.Monad.Trans.State
import Data.Word
import Data.Char (ord, chr)
import Data.Sequence as S
import Data.Map as M
import Data.Foldable as F
import Data.Binary.Put
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.ByteString.Lazy.Char8 (ByteString, unpack, split)
import Data.Int
import Data.List (intercalate)

import ASM.Datatypes

ascii :: Char -> Word8
ascii = fromIntegral . ord

{-
emit :: [Word8] -> ASM ()
emit = emits . Prelude.map SWord8

emits :: [ASMEmit String] -> ASM ()
emits xs = do
    s <- get
    modify $ append $ ASMEmit (asm_offset s) xs
-}

emitStringRef :: String -> ASM ()
emitStringRef x = bemit [SStrRef64 x]

emitString :: String -> ASM ()
emitString s = bemit (Prelude.map (SWord8 . ascii) s)

emitProgSize64 :: ASM ()
emitProgSize64 = bemit [SProgSize64]

-- emitWord64LE :: Word64 -> ASM ()
-- emitWord64LE = emit . bytes putWord64le

-- Append label ref to buffer
emitLabelRef64 :: String -> ASM ()
emitLabelRef64 lbl = bemit [SProgLabel64 lbl]-- emits [SProgLabel64 lbl]

emitLabelOff8 :: String -> ASM ()
emitLabelOff8 lbl = do
    asm_s <- get
    -- emits [SRelOffsetToLabel8 (asm_offset asm_s) lbl]
    bemit [SRelOffsetToLabel8 (asm_offset asm_s) lbl]

emitLabelOff32 :: String -> ASM ()
emitLabelOff32 lbl = do
    asm_s <- get
    -- emits [SRelOffsetToLabel32 (asm_offset asm_s) lbl]
    bemit [SRelOffsetToLabel32 (asm_offset asm_s) lbl]

bytes :: Integral a => (a -> Put) -> a -> [Word8]
bytes putFun x = Prelude.map BS.c2w bs
  where
    bs = unpack $ runPut $ putFun (fromIntegral x)

-- Buffered emit which doesn't emit an immediate command, 
-- but places it in a buffer. bflush does a regular emit.
-- The offset is updated for each of the ASMEmits.
bemit :: [ASMEmit String] -> ASM ()
bemit xs = do
    mapM bemit1 xs
    return ()
{-
  where 
    new :: [ASMEmit String] -> ASMState -> ASMState
    new appendEmits state = 
        state { 
            asm_ebuf   = asm_ebuf state >< S.fromList appendEmits
        ,   asm_offset = asm_offset 
        }
-}

bemit1 :: ASMEmit String -> ASM () 
bemit1 emit = modify (new emit)
  where
    new :: ASMEmit String -> ASMState -> ASMState
    new appendEmit state = 
        state {
            asm_ebuf   = asm_ebuf   state |> appendEmit
        ,   asm_offset = asm_offset state + fromIntegral (lenBytes appendEmit)
        }


-- Flush the buffer and empty it.
bflush :: ASM ()
bflush = do
    s   <- get
    let buf  = asm_ebuf s 
    let boff = asm_ebuf_off s -- the offset BEFORE the emits in the buffer
    let off  = asm_offset s -- the offset just AFTER the emits in the buff
    let pre  = asm_instr s
    modify (\s -> s { asm_instr    = pre |> ASMEmit boff (F.toList buf) })
    modify (\s -> s { asm_ebuf     = S.empty 
                    , asm_ebuf_off = off
                    })

append :: ASMCode -> ASMState -> ASMState
append (ASMEmit _ _) _ = error "Please don't append ASMEmit"
append word state = state { asm_instr  = asm_instr  state |> word
                          -- , asm_offset = asm_offset state + lbw
                          }
    -- where lbw = fromIntegral $ lenBytesCode word

instance Documentation (ASM ()) where
    doc str = do 
        s <- get
        modify $ append $ ASMDoc (asm_offset s) str

documentation :: String -> ASM ()
documentation txt = doc txt {- do
    s <- get
    modify $ append $ ASMDoc (asm_offset s) txt -}

setLabel :: String -> ASM ()
setLabel txt = do
    s <- get
    modify $ append $ ASMLabel (asm_offset s) txt

freshLabelWithPrefix :: String -> ASM String
freshLabelWithPrefix prefix = do
    s <- get
    let i = asm_uid s
    modify new
    return $ prefix ++ show i
    where new s = s { asm_uid = asm_uid s + 1 }

-- Indicates that a string should be added to the strings table
addString :: String -> ASM ()
addString str = modify addString_
    where addString_ s = s { asm_strs = M.insert str 0 (asm_strs s)
                           }

-- Debugging
assertOffsetIs :: Word64 -> String -> ASM ()
assertOffsetIs offset msg = do
    s <- get
    if asm_offset s /= offset then
        throwError $ 
            "Unexpected offset: " ++ 
            (show $ asm_offset s) ++ 
            " expecting: " ++
            (show offset) ++
            " (" ++ msg ++ ")"
    else
        return ()

assemble :: ASM b -> Either String ASMState
assemble = runExcept . flip execStateT (istate 0x00)

-- Display an address, used in an error message
showWord64 :: Word64 -> String
showWord64 w = intercalate " " $ Prelude.map showWord8 bs
    where bs = bytes putWord64be w

-- TODO: Where does this belong? Not here I think, as it's about
-- showing data on screen by the compiler such as in hex binary output
showWord8 :: Word8 -> String
showWord8 w = hex h:hex l:[]
  where
    (h, l) = w `divMod` 16
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

{-
emitString :: String -> ASM ()
emitString str = do
    -- Associate string s with the current offset in the 
    -- strings table:
    modify associateStringOffset
    emitString str
    where associateStringOffset s = s {
        asm_strs = M.insert str (asm_offset s) (asm_strs s)
    }
-}
max64BitUnsigned = fromIntegral (maxBound::Word64)
max32BitUnsigned = fromIntegral (maxBound::Word32)

max8BitValI :: Integer
max8BitValI = fromIntegral (maxBound::Int8)

min8BitValI :: Integer
min8BitValI = fromIntegral (minBound::Int8)
    
emitStrings :: ASM () 
emitStrings = do
    s <- get
    mapM emitString (keys $ asm_strs s)
    return ()

----------------------
-- LABEL RESOLUTION --
----------------------

-- Some of this is boilerplate code but it generally replaces
-- labels with actual bytes, which humans would have a hard
-- time doing.

-- Traverse the code built so far, and return a map of labeled addresses
getLabeledAddresses :: ASM (M.Map String Word64)
getLabeledAddresses = do
    s <- get
    return $ F.foldr buildMap M.empty (asm_instr s)
    where buildMap :: ASMCode -> M.Map String Word64 -> M.Map String Word64
          buildMap (ASMLabel offset s) m = M.insert s offset m
          buildMap _                   m = m

-- Produce a 64 bit absolute address in virtual memory from an absolute
-- address in the program data
-- This is x86 and byte-order dependent! Maybe it belongs to the X86 module.
replaceWithAbsVMemAddr64 :: String -> Word64 -> Word64 -> ASM [Word8]
replaceWithAbsVMemAddr64 label vmemOffset64 labelAddr64
    | vmemOffset64 + labelAddr64 <= max64BitUnsigned = 
        return $ bytes putWord64le $ vmemOffset64 + labelAddr64
replaceWithAbsVMemAddr64 label _ _ =
    error $ "A 64-bit label reference is outside of memory bounds: " ++ label

replaceWithAbsVMemAddr32 :: String -> Word64 -> Word64 -> ASM [Word8]
replaceWithAbsVMemAddr32 label vmemOffset64 labelAddr64
    | vmemOffset64 + labelAddr64 <= max32BitUnsigned = 
        return $ bytes putWord32le $ fromIntegral $ vmemOffset64 + labelAddr64
replaceWithAbsVMemAddr32 label _ _ =
    error $ "A 32-bit label reference is outside of memory bounds: " ++ label

-- Produce a 32 bit signed relative address from a label 
-- reference to the label definition position.
replaceWithRelOffsetToLabel32 label labelAddr64 refAddr64 = 
    return $ bytes putWord32le $ fromIntegral $ labelAddr64 - refAddr64

-- Produce an 8 bit signed relative address from a label 
-- reference to the label definition position.
replaceWithRelOffsetToLabel8 _ labelAddr64 refAddr64
    | min8BitValI <= delta && delta <= max8BitValI =
        return $ bytes putWord8 $ fromIntegral $ delta
    where 
        delta :: Integer
        delta = fromIntegral $ labelAddr64 - refAddr64
replaceWithRelOffsetToLabel8 label labelAddr64 refAddr64 = 
    error $
        "An 8-bit offset to label is out of bounds: " ++ label ++ "\n" 
     ++ "The label points to address " ++ 
            showWord64 labelAddr64 ++ "\n"
     ++ "The source address is       " ++ 
            showWord64 refAddr64 ++ "\n"
        
-- Replace Labels with Bytes 
-- resolves label references with bound checks.
-- Folds down ASMEmit structures (in the ASM monad because it may fail)
replaceLabelEmits
  :: Word64
  -> M.Map String Word64
  -> ASMEmit String
  -> [ASMEmit String] 
  -> ASM [ASMEmit String]
replaceLabelEmits vmemOff64 addrs emit rest =
    case emitHasLabel emit of 
      Nothing -> return $ emit:rest
      Just la ->
        case M.lookup la addrs of
          Nothing    -> error $ "Label reference not found: " ++ la
          Just lbl64 -> do
            replacementEmits <- repl lbl64 emit
            return $ emit:Prelude.map SWord8 replacementEmits ++ rest
    where 
        repl lbl64 (SProgLabel64 l) =
            replaceWithAbsVMemAddr64 l vmemOff64 lbl64
        repl lbl64 (SProgLabel32 l) =
            replaceWithAbsVMemAddr32 l vmemOff64 lbl64
        repl lbl64 (SRelOffsetToLabel32 currOff64 l) =
            -- The offset is calculated from just AFTER the label,
            -- i.e. from currOff64 plus the label reference length,
            -- which for 32 bits is 4.
            replaceWithRelOffsetToLabel32 l lbl64 (currOff64 + 4)
        repl lbl64 (SRelOffsetToLabel8 currOff64 l) =
            replaceWithRelOffsetToLabel8 l lbl64 (currOff64 + 1)

-- Replace label references.
replaceLabelsCode :: Word64              -- Virtual memory offset
                  -> M.Map String Word64 -- Label addresses
                  -> ASMCode
                  -> S.Seq ASMCode 
                  -> ASM (S.Seq ASMCode)
replaceLabelsCode vmemOffset addrs (ASMEmit o es) rest = do
    k <- foldrM f [] es
    return $ ASMEmit o k <| rest
    where f = replaceLabelEmits vmemOffset addrs
replaceLabelsCode _ _ any rest = return $ any <| rest

replaceLabelsWithBytes :: Word64 -> ASM ()
replaceLabelsWithBytes vmemOffset = do
    s        <- get
    labelMap <- getLabeledAddresses
    modify (\s -> s { asm_lbls = labelMap })
    new_asm_instr <- foldrM 
        (replaceLabelsCode vmemOffset labelMap) 
        S.empty 
        (asm_instr s)
    modify (\s -> s { asm_instr = new_asm_instr })
    return ()

-----------------------------
-- Program size resolution --
-----------------------------

replaceProgSizeWithBytes :: ASM ()
replaceProgSizeWithBytes = do
    s <- get
    new_asm_instr <- foldrM 
        (replaceProgSizeCode (asm_offset s))
        S.empty
        (asm_instr s)
    modify (\s -> s { asm_instr = new_asm_instr })

replaceProgSizeCode :: Word64 -> ASMCode -> S.Seq ASMCode -> ASM (S.Seq ASMCode)
replaceProgSizeCode progSize (ASMEmit o es) rest = do
    k <- foldrM f [] es
    return $ ASMEmit o k <| rest
  where 
    f = replaceProgSizeEmit progSize
replaceProgSizeCode _ any rest = return $ any <| rest

replaceProgSizeEmit :: Word64 
                    -> ASMEmit String
                    -> [ASMEmit String] 
                    -> ASM [ASMEmit String]
replaceProgSizeEmit progSize (SProgSize64) rest = 
    return $ SProgSize64 : words ++ rest
  where
    words = Prelude.map SWord8 (bytes putWord64le $ fromIntegral $ progSize)
replaceProgSizeEmit progSize emit rest = return $ emit:rest
