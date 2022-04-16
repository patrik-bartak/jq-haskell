module Jq.Filters where

import Jq.Json

data Optionality = Opt | Req
  deriving (Eq)

data DataType = Arr | Dict

data Filter
  = Identity -- done
  | Paren Filter -- done
  {- most done - check if name parsing and edge cases with and without identity (.foo vs foo) are ok -}
  | DictIdenIndexing Optionality String
  | DictGenIndexing Optionality [String]
  | ArrayIndexing Optionality Filter -- can be filter
  | ArraySlice Optionality (Maybe Filter) (Maybe Filter) -- can be filter
  | Iterator Optionality [Int] -- can be filter
  | RecDesc -- done
  | Pipe Filter Filter -- done
  | Comma Filter Filter -- done
  -- Value constructors (not done)
  | JNullFilter JSON
  | JNumberFilter JSON
  | JStringFilter JSON
  | JBoolFilter JSON
  | JArrayFilter [Filter]
  | JObjectFilter JSON
  -- | JObjectFilter [(String, JSON)]
  -- Conditionals and comparisons
  | Equals Filter Filter
  | NotEquals Filter Filter
  | LessThan Filter Filter
  | LessThanOrEqual Filter Filter
  | MoreThan Filter Filter
  | MoreThanOrEqual Filter Filter
  | IfThenElse Filter Filter Filter
  | LogicalAnd Filter Filter
  | LogicalOr Filter Filter
  | LogicalNot
  -- Arithmetic
  | Add Filter Filter
  | Subt Filter Filter
  | Mult Filter Filter
  | Div Filter Filter
  deriving (Eq)

instance Show Filter where
  -- Identity, Parentheses
  show Identity = "."
  show (Paren filt) = "(" ++ show filt ++ ")"
  -- Dict Indexing
  show (DictIdenIndexing Req field) = "" ++ field
  show (DictIdenIndexing Opt field) = "" ++ field ++ "?"
  -- Dict Generic Indexing
  show (DictGenIndexing Req fields) = show fields
  show (DictGenIndexing Opt fields) = show fields ++ "?"
  -- Array Indexing
  show (ArrayIndexing Req idx) = "[" ++ show idx ++ "]"
  show (ArrayIndexing Opt idx) = "[" ++ show idx ++ "]?"
  -- Array Slicing
  show (ArraySlice Req lo hi) = "[" ++ show lo ++ ":" ++ show hi ++ "]"
  show (ArraySlice Opt lo hi) = "[" ++ show lo ++ ":" ++ show hi ++ "]?"
  -- Iterators
  show (Iterator Req idxs) = show idxs
  show (Iterator Opt idxs) = show idxs ++ "?"
  -- RecDesc
  show RecDesc = ".."
  -- Pipe, Comma
  show (Pipe f1 f2) = show f1 ++ "|" ++ show f2
  show (Comma f1 f2) = show f1 ++ "," ++ show f2
  -- Value constructors
  show (JNullFilter n) = show n
  show (JNumberFilter n) = show n
  show (JStringFilter s) = show s
  show (JBoolFilter b) = show b
  show (JArrayFilter fs) = show fs
  show (JObjectFilter flds) = show flds
  -- Conditionals and comparisons
  show (Equals f1 f2) = show f1 ++ " == " ++ show f2
  show (NotEquals f1 f2) = show f1 ++ " != " ++ show f2
  show (LessThan f1 f2) = show f1 ++ " < " ++ show f2
  show (LessThanOrEqual f1 f2) = show f1 ++ " <= " ++ show f2
  show (MoreThan f1 f2) = show f1 ++ " > " ++ show f2
  show (MoreThanOrEqual f1 f2) = show f1 ++ " >= " ++ show f2
  show (IfThenElse cond tru fals) = "if (" ++ show cond ++ ") then (" ++ show tru ++ ") else (" ++ show fals ++ ") end"
  show (LogicalAnd f1 f2) = show f1 ++ " and " ++ show f2
  show (LogicalOr f1 f2) = show f1 ++ " or " ++ show f2
  show LogicalNot = "not"
  -- Arithmetic
  show (Add f1 f2 ) = show f1 ++ " + " ++ show f2
  show (Subt f1 f2 ) = show f1 ++ " - " ++ show f2
  show (Mult f1 f2 ) = show f1 ++ " * " ++ show f2
  show (Div f1 f2 ) = show f1 ++ " / " ++ show f2


-- instance Eq Filter where
--   -- Identity, Parentheses
--   Identity == Identity                              = True
--   (Paren f1) == (Paren f2)                          = f1 == f2
--   -- Dict Indexing
--   (DictIdenIndexing field1) == (DictIdenIndexing field2)    = field1 == field2
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
filterIndexingSC = DictIdenIndexing Req

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = Pipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = Comma
