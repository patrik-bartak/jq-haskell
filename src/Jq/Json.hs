module Jq.Json where

data JSON = JNull
          | JNumber (Either Int Float)
          | JString String
          | JBool Bool
          | JArray [JSON]
          | JObject [(String, JSON)]

renderString :: String -> String
renderString str = "\"" ++ escape str ++ "\""
    where escape [] = []
          escape (x:y:xs)
                    | x == '\\' && y == 'n' = y : escape xs
                    | x == '\\' && y == '\\' = x : y : escape xs
                    | otherwise = x : y : escape xs
          escape xs = xs

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
renderInnerJArray (x:xs) = "\n" ++ show x ++ "," ++ renderInnerJArray xs

renderInnerJObject :: (Show a, Show b) => [(a, b)] -> String
renderInnerJObject [] = ""
renderInnerJObject [(key, val)] = "\n" ++ show key ++ ": " ++ show val ++ "\n"
renderInnerJObject ((key, val):xs) = "\n" ++ show key ++ ": " ++ show val ++ "," ++ renderInnerJObject xs

replaceExceptLast :: Char -> String -> String -> String
replaceExceptLast _ _ [] = []
replaceExceptLast _ _ [x] = [x]
replaceExceptLast old new (x:xs) = if x == old then new ++ replaceExceptLast old new xs else x : replaceExceptLast old new xs

-- renderJArray :: String
-- renderJArray = "[" ++ replace "\n" "\n  " (renderInnerJArray xs) ++ "]"

mergeSort :: Ord a => ((a, b) -> (a, b) -> Ordering) -> [(a, b)] -> [(a, b)]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort f xs = merge f (mergeSort f firstHalf) (mergeSort f secondHalf)
  where half = div (length xs) 2
        firstHalf = take half xs
        secondHalf = drop half xs

merge :: Ord a => ((a, b) -> (a, b) -> Ordering) -> [(a, b)] -> [(a, b)] -> [(a, b)]
merge _ [] [] = []
merge _ xs [] = xs
merge _ [] ys = ys
merge f (x:xs) (y:ys)
  | f x y == LT = x : merge f xs (y:ys)
  | otherwise = y : merge f (x:xs) ys

instance Show JSON where
  show JNull = "null"
  show (JNumber (Left n)) = show n
  show (JNumber (Right n)) = show n
  show (JString str) = renderString str
  show (JBool b) = renderBool b
  show (JArray xs) = "[" ++ replaceExceptLast '\n' "\n  " (renderInnerJArray xs) ++ "]"
  show (JObject objs) = "{" ++ sorted ++ "}"
          where sorted =  replaceExceptLast '\n' "\n  " (renderInnerJObject (mergeSort sortFunction objs))
                sortFunction = \x y -> compare (fst x) (fst y)

-- >>> show (jsonObjectSC [("key", JNumber (Right 2.3e23))])
-- "{\n  \"key\": 2.3e23\n}"
-- >>> show (jsonArraySC [jsonNullSC, (jsonArraySC [jsonNullSC])])
-- "[\n  null,\n  [\n    null\n  ]\n]"
instance Eq JSON where
  JNull == JNull = True
  (JNumber (Left x)) == (JNumber (Left y)) = x == y
  (JNumber (Right x)) == (JNumber (Right y)) = x == y
  (JString x) == (JString y) = x == y
  (JBool x) == (JBool y) = x == y
  (JArray xs) == (JArray ys) = xs == ys
  (JObject xs) == (JObject ys) = xs == ys
  _ == _ = False

-- Smart constructors
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC n = JNumber (Left n)

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC = JArray

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = JObject
