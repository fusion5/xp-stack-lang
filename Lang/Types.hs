module Lang.Types where

data BaseType = 
    TyWord     -- 64 bits of data
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
