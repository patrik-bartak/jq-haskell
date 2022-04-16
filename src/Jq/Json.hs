module Jq.Json where
import Data.Char
import Data.List (isPrefixOf)


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
  --   escape (x : y : toSplit)
  --     | x == '\\' && y == 'n' = y : escape toSplit
  --     | x == '\\' && y == '\\' = x : y : escape toSplit
  --     | otherwise = x : y : escape toSplit
  --   escape toSplit = toSplit

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
renderInnerJArray (x : toSplit) = "\n" ++ show x ++ "," ++ renderInnerJArray toSplit

renderInnerJObject :: [(String, JSON)] -> String
renderInnerJObject [] = ""
renderInnerJObject [(key, val)] = "\n" ++ show (JString key) ++ ": " ++ show val ++ "\n"
renderInnerJObject ((key, val) : toSplit) = "\n" ++ show (JString key) ++ ": " ++ show val ++ "," ++ renderInnerJObject toSplit

replaceExceptLast :: Char -> String -> String -> String
replaceExceptLast _ _ [] = []
replaceExceptLast _ _ [x] = [x]
replaceExceptLast old new (x : toSplit) = if x == old then new ++ replaceExceptLast old new toSplit else x : replaceExceptLast old new toSplit

escapeChars :: [Char] -> String -> String
escapeChars [] str = str
escapeChars (old : olds) str = escapeChars olds (replace old new str)
  where
    new = if old == '"' then "\\\"" else showLitChar old ""

replace :: Char -> String -> String -> String
replace _ _ [] = []
replace old new ('\\' : 'u' : '0' : '0' : '0' : '0' : rest) = "\\u0000" ++ replace old new rest
replace old new (x : toSplit) = if x == old then new ++ replace old new toSplit else x : replace old new toSplit

-- >>> escapeChars "" "\\u0000"
-- "\\u0000"

-- renderJArray :: String
-- renderJArray = "[" ++ replace "\n" "\n  " (renderInnerJArray toSplit) ++ "]"

mergeSort :: Ord a => ((a, b) -> (a, b) -> Ordering) -> [(a, b)] -> [(a, b)]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort f toSplit = merge f (mergeSort f firstHalf) (mergeSort f secondHalf)
  where
    half = div (length toSplit) 2
    firstHalf = take half toSplit
    secondHalf = drop half toSplit

merge :: Ord a => ((a, b) -> (a, b) -> Ordering) -> [(a, b)] -> [(a, b)] -> [(a, b)]
merge _ [] [] = []
merge _ toSplit [] = toSplit
merge _ [] ys = ys
merge f (x : toSplit) (y : ys)
  | f x y == LT = x : merge f toSplit (y : ys)
  | otherwise = y : merge f (x : toSplit) ys

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
  show (JArray toSplit) = "[" ++ replaceExceptLast '\n' "\n  " (renderInnerJArray toSplit) ++ "]"
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
  (JArray toSplit) == (JArray ys) = toSplit == ys
  (JObject toSplit) == (JObject ys) = toSplit == ys
  _ == _ = False

instance Ord JSON where
  compare JNull JNull = EQ
  compare (JNumber x) (JNumber y) = compare x y
  compare (JString x) (JString y) = compare x y
  compare (JBool x) (JBool y) = compare x y
  compare (JArray toSplit) (JArray ys) = compare toSplit ys
  compare (JObject toSplit) (JObject ys) = compare toSplit ys
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


removeAll :: Eq a => [a] -> [a] -> [a]
removeAll = foldr removeEveryInstanceOfFrom

removeEveryInstanceOfFrom :: Eq a => a -> [a] -> [a]
removeEveryInstanceOfFrom _ [] = []
removeEveryInstanceOfFrom toRemove (x:toSplit)
  | x == toRemove = removeEveryInstanceOfFrom toRemove toSplit
  | otherwise = x : removeEveryInstanceOfFrom toRemove toSplit

-- >>> [1,2,3,5,5,3,3,3,3,3,3,4,4,4,2,1,1,1,2,2,2,7,8,9,0] `removeAll` [1,2,3,3,4,7,7,7,0]
-- [5,5,8,9]

tileListDub :: Double -> [a] -> [a]
tileListDub n = tileList (floor n)

tileList :: Int -> [a] -> [a]
tileList n toSplit
  | n > 0 = toSplit ++ tileList (n - 1) toSplit
  | otherwise = []

recMergeKvPairIntoKvPairList :: (String, JSON) -> [(String, JSON)] -> [(String, JSON)]
recMergeKvPairIntoKvPairList toMerge [] = [toMerge]
recMergeKvPairIntoKvPairList (toMergeKey,toMergeVal) ((kvpKey,kvpVal):kvps)
  | toMergeKey == kvpKey = case (kvpVal, toMergeVal) of
    (JObject toSplit, JObject ys) -> (toMergeKey, JObject (recMergeKvps toSplit ys)) : kvps
    (_, _)                   -> (toMergeKey,toMergeVal) : kvps
  | otherwise = (kvpKey,kvpVal) : recMergeKvPairIntoKvPairList (toMergeKey,toMergeVal) kvps

recMergeKvps :: [(String, JSON)] -> [(String, JSON)] -> [(String, JSON)]
recMergeKvps kvps1 [] = kvps1
recMergeKvps [] kvps2 = kvps2
recMergeKvps kvps1 (kvp2:kvps2) = recMergeKvps listWithOneMerged kvps2
  where listWithOneMerged = recMergeKvPairIntoKvPairList kvp2 kvps1

mergeKvPairIntoKvPairList :: (String, JSON) -> [(String, JSON)] -> [(String, JSON)]
mergeKvPairIntoKvPairList toMerge [] = [toMerge]
mergeKvPairIntoKvPairList (toMergeKey,toMergeVal) ((kvpKey,kvpVal):kvps)
  | toMergeKey == kvpKey = (toMergeKey,toMergeVal) : kvps
  | otherwise = (kvpKey,kvpVal) : mergeKvPairIntoKvPairList (toMergeKey,toMergeVal) kvps

mergeKvps :: [(String, JSON)] -> [(String, JSON)] -> [(String, JSON)]
mergeKvps kvps1 [] = kvps1
mergeKvps [] kvps2 = kvps2
mergeKvps kvps1 (kvp2:kvps2) = mergeKvps listWithOneMerged kvps2
  where listWithOneMerged = mergeKvPairIntoKvPairList kvp2 kvps1

-- recMergeKvps :: [(String, JSON)] -> [(String, JSON)] -> [(String, JSON)]
-- recMergeKvps kvps1 kvps2 = toList (fromList kvps2 `union` fromList kvps1)

-- >>> tileListDub (3.6) "tile"
-- "tiletiletile"

-- https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-Num.html
instance Num JSON where
  -- Addition
  anyThing + JNull = anyThing
  JNull + anyThing = anyThing
  (JNumber x) + (JNumber y) = JNumber (x + y)
  (JString toSplit) + (JString ys) = JString (toSplit ++ ys)
  (JArray toSplit) + (JArray ys) = JArray (toSplit ++ ys)
  (JObject kvps1) + (JObject kvps2) = JObject (mergeKvps kvps1 kvps2)
  _ + _ = error "Error performing addition between different types"
  -- Subtraction
  (JNumber x) - (JNumber y) = JNumber (x - y)
  (JArray toSplit) - (JArray ys) = JArray (toSplit `removeAll` ys)
  _ - _ = error "Error performing subtraction between different types"
  -- Multiplication -> https://stedolan.github.io/jq/manual/v1.5/#Builtinoperatorsandfunctions
  (JNumber x) * (JNumber y) = JNumber (x * y)
  (JNumber n) * (JString str)
    | n <= 0 = JNull
    | otherwise = JString (tileListDub n str)
  (JString str) * (JNumber n) = JNumber n * JString str
  (JObject kvps1) * (JObject kvps2) = JObject (recMergeKvps kvps1 kvps2)
  _ * _ = error "Error performing multiplication between different types"
  -- JNull
  -- JNumber
  -- JString
  -- JBool
  -- JArray
  -- JObject

indicesOf :: String -> String -> Int -> [Int]
indicesOf toSplit sep = indicesOfHelper (toSplit ++ sep) sep

indicesOfHelper :: String -> String -> Int -> [Int]
indicesOfHelper [] _ _ = []
indicesOfHelper toSplit sep idx
  | sep == "" = idx : indicesOfHelper (tail toSplit) sep 0
  | sep `isPrefixOf` toSplit = idx : indicesOfHelper nextSegment sep 0
  | otherwise = indicesOfHelper startingFromSep sep (idx + numDropped)
    where sepLen = length sep
          nextSegment = drop sepLen toSplit
          numDropped = length (takeWhilePrefix toSplit sep)
          startingFromSep = dropWhilePrefix toSplit sep

takeWhilePrefix :: String -> String -> String
takeWhilePrefix [] _ = []
takeWhilePrefix (o:other) sep
  | sep `isPrefixOf` (o:other) = []
  | otherwise = o : takeWhilePrefix other sep

dropWhilePrefix :: String -> String -> String
dropWhilePrefix [] _ = []
dropWhilePrefix (o:other) sep
  | sep `isPrefixOf` (o:other) = o:other
  | otherwise = dropWhilePrefix other sep

splitByHelper :: [a] -> [Int] -> Int -> [[a]]
splitByHelper _ [] _ = error "Not possible"
splitByHelper toSplit [idx] _ = [take idx toSplit]
splitByHelper toSplit (idx:idxs) skiplen
  = taken : splitByHelper (drop skiplen left) idxs skiplen
    where taken = take idx toSplit
          left = drop idx toSplit
splitBy :: [Char] -> [Char] -> JSON
splitBy toSplit sep = JArray (map JString splitResult)
  where splitResult = splitByHelper toSplit (indicesOf toSplit sep 0) (length sep)
-- >>> indicesOf "asdasd" "as" 0
-- [0,1,1]

-- >>> splitBy "xadsfasdffdsxcasdfxdsfasdfxadsfasdfxsd" "asdf"

-- https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#t:Fractional
instance Fractional JSON where
  -- Division
  (JNumber x) / (JNumber y) = JNumber (x / y)
  (JNumber _) / (JNumber 0) = error "Error performing division by zero"
  (JString x) / (JString y) = x `splitBy` y
  _ / _ = error "Error performing divion between different types"


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
