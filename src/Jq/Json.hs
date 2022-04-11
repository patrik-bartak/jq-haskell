module Jq.Json where
import Data.Char

data JSON
  = JNull
  | JNumber Double
  | JString String
  | JBool Bool
  | JArray [JSON]
  | JObject [(String, JSON)]
    -- deriving Show

  -- where
  --   escape [] = []
  --   escape (x : y : xs)
  --     | x == '\\' && y == 'n' = y : escape xs
  --     | x == '\\' && y == '\\' = x : y : escape xs
  --     | otherwise = x : y : escape xs
  --   escape xs = xs

-- >>> :t 2.0e45
-- 2.0e45 :: Fractional p => p
-- >>> renderString "\n"
-- "\"\n\""
-- >>> renderString "\\"
-- "\"\\\""
-- >>> show (JArray [JString "foo",JNumber 2,JNumber 3])
-- "[\n  \"foo\",\n  2,\n  3\n]"

-- want:
-- '\"' : "\n" ++ "\""
-- "\"\\\""

renderBool :: Bool -> String
renderBool True = "true"
renderBool False = "false"

renderInnerJArray :: Show a => [a] -> String
renderInnerJArray [] = ""
renderInnerJArray [x] = "\n" ++ show x ++ "\n"
renderInnerJArray (x : xs) = "\n" ++ show x ++ "," ++ renderInnerJArray xs

renderInnerJObject :: (Show a, Show b) => [(a, b)] -> String
renderInnerJObject [] = ""
renderInnerJObject [(key, val)] = "\n" ++ show key ++ ": " ++ show val ++ "\n"
renderInnerJObject ((key, val) : xs) = "\n" ++ show key ++ ": " ++ show val ++ "," ++ renderInnerJObject xs

replaceExceptLast :: Char -> String -> String -> String
replaceExceptLast _ _ [] = []
replaceExceptLast _ _ [x] = [x]
replaceExceptLast old new (x : xs) = if x == old then new ++ replaceExceptLast old new xs else x : replaceExceptLast old new xs

escapeChars :: [Char] -> String -> String
escapeChars [] str = str
escapeChars (old : olds) str = escapeChars olds (replace old new str)
  where
    new = if old == '"' then "\\\"" else showLitChar old ""

replace :: Char -> String -> String -> String
replace _ _ [] = []
replace old new (x : xs) = if x == old then new ++ replace old new xs else x : replace old new xs

-- >>> escapeChars "\n\r\f" "asdf\n\r\f"
-- "asdf\\n\\r\\f"

-- renderJArray :: String
-- renderJArray = "[" ++ replace "\n" "\n  " (renderInnerJArray xs) ++ "]"

mergeSort :: Ord a => ((a, b) -> (a, b) -> Ordering) -> [(a, b)] -> [(a, b)]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort f xs = merge f (mergeSort f firstHalf) (mergeSort f secondHalf)
  where
    half = div (length xs) 2
    firstHalf = take half xs
    secondHalf = drop half xs

merge :: Ord a => ((a, b) -> (a, b) -> Ordering) -> [(a, b)] -> [(a, b)] -> [(a, b)]
merge _ [] [] = []
merge _ xs [] = xs
merge _ [] ys = ys
merge f (x : xs) (y : ys)
  | f x y == LT = x : merge f xs (y : ys)
  | otherwise = y : merge f (x : xs) ys

instance Show JSON where
  show JNull = "null"
  show (JNumber dub)
    -- Takes care of funky JSON number formatting like 3.0 -> 3 and 3.e40 -> 3e40
    | shouldBeDisplayedAsInt dubStrWithoutExp = asIntWithFixedEnding
    | otherwise = asFloatWithFixedEnding
    where
      dubStr = show dub
      dubStrAsInt = getBeforeChar '.' dubStr
      dubStrWithoutExp = getBeforeChar 'e' dubStr
      possibleExpEnding = getFromChar 'e' dubStr
      fixedExpEnding = fixSignBeforeExponential possibleExpEnding
      asIntWithFixedEnding = dubStrAsInt ++ fixedExpEnding
      asFloatWithFixedEnding = dubStrWithoutExp ++ fixedExpEnding
  show (JString str) = "\"" ++ escapeChars ['\\', '\'','\"','\n','\r','\t','\b','\f','\v'] str ++ "\""
  show (JBool b) = renderBool b
  show (JArray xs) = "[" ++ replaceExceptLast '\n' "\n  " (renderInnerJArray xs) ++ "]"
  show (JObject objs) = "{" ++ sorted ++ "}"
    where
      sorted = replaceExceptLast '\n' "\n  " (renderInnerJObject (mergeSort sortFunction objs))
      -- sorted = replaceExceptLast '\n' "\n  " (renderInnerJObject objs)
      sortFunction = \x y -> compare (fst x) (fst y)

fixSignBeforeExponential :: [Char] -> [Char]
fixSignBeforeExponential [] = []
fixSignBeforeExponential [_] = error "This should never happen"
fixSignBeforeExponential (e : digitOrMinus : rest)
  | digitOrMinus == '-' = e : digitOrMinus : rest
  | otherwise = e : '+' : digitOrMinus : rest

getBeforeChar :: Char -> [Char] -> [Char]
getBeforeChar c = takeWhile (/= c)

getFromChar :: Char -> [Char] -> [Char]
getFromChar c = dropWhile (/= c)

endingOfDoublesThatAreActuallyInts :: [Char]
endingOfDoublesThatAreActuallyInts = '.' : repeat '0'

shouldBeDisplayedAsInt :: String -> Bool
shouldBeDisplayedAsInt floatStr = all (uncurry (==)) (zip ending endingOfDoublesThatAreActuallyInts)
  where
    ending = getFromChar '.' floatStr

-- >>> read "2." :: Double
-- Prelude.read: no parse
-- >>> show (JNumber 10)
-- "10"

-- >>> show (jsonObjectSC [("key", JNumber (Right 2.3e23))])
-- "{\n  \"key\": 2.3e23\n}"
-- >>> show (jsonArraySC [jsonNullSC, (jsonArraySC [jsonNullSC])])
-- "[\n  null,\n  [\n    null\n  ]\n]"
instance Eq JSON where
  JNull == JNull = True
  (JNumber x) == (JNumber y) = x == y
  (JString x) == (JString y) = x == y
  (JBool x) == (JBool y) = x == y
  (JArray xs) == (JArray ys) = xs == ys
  (JObject xs) == (JObject ys) = xs == ys
  _ == _ = False

instance Ord JSON where
  compare JNull JNull = EQ
  compare (JNumber x) (JNumber y) = compare x y
  compare (JString x) (JString y) = compare x y
  compare (JBool x) (JBool y) = compare x y
  compare (JArray xs) (JArray ys) = compare xs ys
  compare (JObject xs) (JObject ys) = compare xs ys
  -- Special cases JNull
  compare JNull (JNumber _) = LT
  compare JNull (JString _) = LT
  compare JNull (JBool _) = LT
  compare JNull (JArray _) = LT
  compare JNull (JObject _) = LT
  -- Special cases JNumber
  compare (JNumber _) JNull = GT
  compare (JNumber _) (JString _) = LT
  compare (JNumber _) (JBool _) = GT
  compare (JNumber _) (JArray _) = LT
  compare (JNumber _) (JObject _) = LT
  -- Special cases JNumber
  compare (JString _) JNull = GT
  compare (JString _) (JNumber _) = GT
  compare (JString _) (JBool _) = GT
  compare (JString _) (JArray _) = LT
  compare (JString _) (JObject _) = LT
  -- Special cases JBool
  compare (JBool _) JNull = GT
  compare (JBool _) (JNumber _) = LT
  compare (JBool _) (JString _) = LT
  compare (JBool _) (JArray _) = LT
  compare (JBool _) (JObject _) = LT
  -- Special cases JArray
  compare (JArray _) JNull = GT
  compare (JArray _) (JNumber _) = GT
  compare (JArray _) (JString _) = GT
  compare (JArray _) (JBool _) = GT
  compare (JArray _) (JObject _) = LT
  -- Special cases JObject
  compare (JObject _) JNull = GT
  compare (JObject _) (JNumber _) = GT
  compare (JObject _) (JString _) = GT
  compare (JObject _) (JBool _) = GT
  compare (JObject _) (JArray _) = GT

-- Smart constructors
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC n = JNumber (fromIntegral n)

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC = JArray

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = JObject
