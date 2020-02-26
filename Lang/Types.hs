module Lang.Types where

import Data.Word

data BaseType = 
    TyWord     -- 64 bits of data, no signed or unsigned property
  | TySigned   -- 64 bit signed integer
  | TyUnsigned -- 64 bit unsigned integer, same as TyWord actually.
    deriving (Eq)

data Type =
    TySeq    -- :type*n:n -- HOMOGENEOUS seq, size n unknown at compile time
        Type Integer      -- TODO: implement.
  | TyProd   -- e.g. :w64:w64
        Type     -- TyEmpty or another TyProd
        BaseType -- Should only ever be a base type. Stack top.
  | TySum    -- e.g. :w64 | :w64:w64 (but unique stack length = max of the two)
        Type
        Type
  | TyFunc
        String -- Function name (for info)
        Type   -- Stack before 
        Type   -- Stack after
  | TyEmpty -- Empty stack type --
  | TyId String  -- Identify a type from a TySum by a name

baseWord = TyProd TyEmpty TyWord

btyBytes :: BaseType -> Word32
btyBytes _ = 8

sizeof :: Type -> Word32
sizeof (TyProd t bt) = btyBytes bt + sizeof t
sizeof (TySum t1 t2) = max (sizeof t1) (sizeof t2)
sizeof (TyEmpty)     = 0
