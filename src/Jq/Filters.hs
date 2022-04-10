module Jq.Filters where

import Jq.Json

data IndexType = Iden | Gen
  deriving (Eq)

data Optionality = Opt | Req
  deriving (Eq)

data DataType = Arr | Dict

data Filter
  = Identity -- done
  | Paren Filter -- done
  {- most done - check if name parsing and edge cases with and without identity (.foo vs foo) are ok -}
  | DictIndexing IndexType Optionality String
  | ArrayIndexing Optionality Int -- done
  | ArraySlice Optionality Int Int -- done
  | Iter Optionality [Int]
  | -- | ArrayIter Optionality [Int]
    -- | DictIter Optionality [Int]
    RecDesc -- done
  | Pipe Filter Filter -- done
  | Comma Filter Filter -- done
  | JsonFilter JSON -- value constructors not done
  deriving (Eq)

instance Show Filter where
  -- Identity, Parentheses
  show Identity = "."
  show (Paren filt) = "(" ++ show filt ++ ")"
  -- Dict Indexing
  show (DictIndexing Iden Req field) = "" ++ field
  show (DictIndexing Iden Opt field) = "" ++ field ++ "?"
  -- Dict Generic Indexing
  show (DictIndexing Gen Req field) = "[\"" ++ field ++ "\"]"
  show (DictIndexing Gen Opt field) = "[\"" ++ field ++ "\"]?"
  -- Array Indexing
  show (ArrayIndexing Req idx) = "[" ++ show idx ++ "]"
  show (ArrayIndexing Opt idx) = "[" ++ show idx ++ "]?"
  -- Array Slicing
  show (ArraySlice Req lo hi) = "[" ++ show lo ++ ":" ++ show hi ++ "]"
  show (ArraySlice Opt lo hi) = "[" ++ show lo ++ ":" ++ show hi ++ "]?"
  -- Iterators
  show (Iter Req idxs) = show idxs
  show (Iter Opt idxs) = show idxs ++ "?"
  -- RecDesc
  show RecDesc = ".."
  -- Pipe, Comma
  show (Pipe f1 f2) = show f1 ++ "|" ++ show f2
  show (Comma f1 f2) = show f1 ++ "," ++ show f2

-- instance Eq Filter where
--   -- Identity, Parentheses
--   Identity == Identity                              = True
--   (Paren f1) == (Paren f2)                          = f1 == f2
--   -- Dict Indexing
--   (DictIndexing field1) == (DictIndexing field2)    = field1 == field2
--   (DictOptIndexing field1) == (DictOptIndexing field2) = field1 == field2
--   -- Dict Generic Indexing
--   (DictGenIndexing field1) == (DictGenIndexing field2) = field1 == field2
--   (DictGenOptIndexing field1) == (DictGenOptIndexing field2) = field1 == field2

--   -- Pipe, Comma
--   (Pipe f1 g1) == (Pipe f2 g2)                      = f1 == f2 && g1 == g2
--   (Comma f1 g1) == (Comma f2 g2)                    = f1 == f2 && g1 == g2
--   _ == _                                            = False

data Config = ConfigC {filters :: Filter}

-- Smart constructors
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterIndexingSC :: String -> Filter
filterIndexingSC = DictIndexing Iden Req

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = Pipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = Comma
