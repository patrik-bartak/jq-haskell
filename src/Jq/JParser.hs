module Jq.JParser where

import Data.Char
import Jq.Json
import Parsing.Parsing
import Debug.Trace
import Numeric

parseJNull :: Parser JSON
parseJNull = do
  _ <- string "null" <|> string "Nan" <|> string "NAn" <|> string "NaN" <|> string "NAN"
  return JNull

-- decInt :: Parser JSON
-- decInt = do
--   val <- int
--   _ <- char '.'
--   _ <- many (char '0')
--   return (JNumber (Left val))

parseJNumberInt :: Parser Int
parseJNumberInt = int

parseJNumberDecimalDouble :: Parser JSON
parseJNumberDecimalDouble = do
  beforeDecimal <- int
  _ <- char '.'
  afterDecimal <- many digit
  -- Haskell cannot read doubles like 2.
  let afterDecimalNeverNull = if null afterDecimal then "0" else afterDecimal
  let doubleAsString = show beforeDecimal ++ "." ++ afterDecimalNeverNull
  return (JNumber (read doubleAsString))

parseJNumberIntyDouble :: Parser JSON
parseJNumberIntyDouble = JNumber . fromIntegral <$> int

parseJNumberExpDouble :: Parser JSON
parseJNumberExpDouble = do
  beforeExp <- parseJNumberDecimalDouble <|> parseJNumberIntyDouble
  _ <- char 'e' <|> char 'E'
  afterExp <- int
  -- Haskell cannot read doubles like 2.
  let doubleAsString = show beforeExp ++ "e" ++ show afterExp
  return (JNumber (read doubleAsString))

parseJNumber :: Parser JSON
parseJNumber =
  parseJNumberExpDouble
    <|> parseJNumberDecimalDouble
    <|> parseJNumberIntyDouble

parseJBool :: Parser JSON
parseJBool = do
  boolStr <- string "true" <|> string "false"
  return (if boolStr == "true" then JBool True else JBool False)

-- >>> parse parseJBool "false"
-- [(false,"")]

parseJArrayOpen :: Parser Char
parseJArrayOpen = char '['

parseJArrayClose :: Parser Char
parseJArrayClose = char ']'

parseJArrayEmpty :: Parser JSON
parseJArrayEmpty = do
  _ <- parseJArrayOpen
  _ <- parseJArrayClose
  return (JArray [])

parseJArraySingleton :: Parser JSON
parseJArraySingleton = do
  _ <- parseJArrayOpen
  singleInnerJson <- parseJSON
  _ <- parseJArrayClose
  return (JArray [singleInnerJson])

parseJArrayMultiple :: Parser JSON
parseJArrayMultiple = do
  _ <- parseJArrayOpen
  singleInnerJson <- parseJSON
  innerJsons <- many parseInnerJsons
  _ <- parseJArrayClose
  return (JArray (singleInnerJson : innerJsons))
  where
    parseInnerJsons = do
      _ <- parseMultiValueSeperator
      parseJSON

parseJArray :: Parser JSON
parseJArray = parseJArrayEmpty <|> parseJArraySingleton <|> parseJArrayMultiple

{- >>> parse parseJArray "[1,2,\"string\",[123,23,false,[1]]]"
[([
  1,
  2,
  "string",
  [
    123,
    23,
    false,
    [
      1
    ]
  ]
],"")]
-}

parseJObjectOpen :: Parser Char
parseJObjectOpen = token (char '{')

parseJObjectClose :: Parser Char
parseJObjectClose = token (char '}')

parseJObjectMapChar :: Parser Char
parseJObjectMapChar = token (char ':')

parseJObjectEmpty :: Parser JSON
parseJObjectEmpty = do
  _ <- parseJObjectOpen
  _ <- parseJObjectClose
  return (JObject [])

parseJObjectField :: Parser String
parseJObjectField = do
  json <- parseJString
  return
    ( case json of
        JString str -> str
        _ -> ""
    )

parseMultiValueSeperator :: Parser Char
parseMultiValueSeperator = token (char ',')

parseJObjectKeyValPair :: Parser (String, JSON)
parseJObjectKeyValPair =
  token
    ( do
        key <- parseJObjectField
        _ <- parseJObjectMapChar
        singleInnerJson <- parseJSON
        return (key, singleInnerJson)
    )

parseJObjectSingleton :: Parser JSON
parseJObjectSingleton = do
  _ <- parseJObjectOpen
  (key, singleInnerJson) <- parseJObjectKeyValPair
  _ <- parseJObjectClose
  return (JObject [(key, singleInnerJson)])

parseJObjectMultiple :: Parser JSON
parseJObjectMultiple = do
  _ <- parseJObjectOpen
  (key, singleInnerJson) <- parseJObjectKeyValPair
  innerJsons <- many parseJObjectInnerJsons
  _ <- parseJObjectClose
  -- Add functino that only keeps last instance of duplicate keys
  -- Maybe some hashset?
  return (JObject ((key, singleInnerJson) : innerJsons))
  where
    parseJObjectInnerJsons = do
      _ <- parseMultiValueSeperator
      parseJObjectKeyValPair

parseJObject :: Parser JSON
parseJObject = parseJObjectEmpty <|> parseJObjectSingleton <|> parseJObjectMultiple

{- >>> parse parseJSON "{\"foo\":[1,2,3,\"bar\",[false, null]]}"
[({
  "foo": [
    1,
    2,
    3,
    "bar",
    [
      false,
      null
    ]
  ]
},"")]
-}

-- unicode :: Parser String
-- unicode = some (sat isLetter <|> sat isSpace <|> sat isMark <|> sat isSymbol <|> sat isSeparator <|> sat isPunctuation)

-- innerJString :: Parser String
-- innerJString = some (alphanum <|> char '_' <|> char '.' <|> char ',' <|> char ' ' <|> char '{' <|> char '}' <|> char ':' <|> sat isSymbol <|> sat isSeparator)
--     -- <|> sat isPrint
-- -- innerJString = some (sat (const True))
-- innerJStringEscaped :: Parser String
-- innerJStringEscaped = string "\\\"" <|> string "\\n" <|> string "\\t" <|> string "\\r" <|> string "\\f" <|> string "\\v" <|> string "\\b"
--                        <|> string "\\\\"

-- readHexParser :: Parser Int
-- readHexParser = do
--   num <- readHex
--   return num

unicode :: Parser String
unicode = do
  _ <- char '\\'
  _ <- char 'u'
  a <- alphanum
  b <- alphanum
  c <- alphanum
  d <- alphanum
  let asInt = fst (head (readHex [a,b,c,d]))
  return [toEnum asInt :: Char]

escape :: Parser String
escape = (do
  escStr <- string "\\'" <|> string "\\\"" <|> string "\\\\"
        <|> string "\\n" <|> string "\\r" <|> string "\\t"
        <|> string "\\b" <|> string "\\f" <|> string "\\v"
        -- should not be in JSON strings?
        -- <|> string "\\0" <|> string "\\xFF"
  return [fst (head (readLitChar escStr))])
      <|> (do 
  s <- string "\\"
  anyChar <- sat (const False)
  return (errorWithoutStackTrace ("Invalid escape sequence: " ++ s ++ [anyChar])))

anyOtherThanEndQuote :: Parser String
anyOtherThanEndQuote = do
  str <- sat ('"' /=)
  return [str]

parseJString :: Parser JSON
parseJString = do
  _ <- char '"'
  -- read anything other than escaped quotes 
  strs <- many (unicode <|> escape <|> anyOtherThanEndQuote)
  _ <- char '"'
  let joined = concat strs
  -- Parse debugging
  -- return (trace (show (zip (map length strs) strs)) (JString joined))
  return (JString joined)

-- >>> length "\\\\\\\""
-- 4

{- >>> parse parseJSON "[\"Aa\r\n\t\b\f\"]"
[([
  "Aa\r\n\t\b\f"
],"")]
-}

-- Does not work
{- >>> parse parseJSON "\"Ну и где этот ваш хвалёный уникод?\""
[("\1053\1091 \1080 \1075\1076\1077 \1101\1090\1086\1090 \1074\1072\1096 \1093\1074\1072\1083\1105\1085\1099\1081 \1091\1085\1080\1082\1086\1076?","")]
-}

-- "\"as\\\"df\"" -> [("as\"df","")] is correct
{- >>> parse parseJString "\"as\\n\\\"df\""
[("as\n\"df","")]
-}

-- Output should have half the number of slashes
-- This works
{- >>> parse parseJSON "\"A\\\\a\\r\\n\\t\\b\\f\""
[("A\\a\r\n\t\b\f","")]
-}

-- This should be the sun
{- >>> parse parseJSON "\"\u2600\""
lexical error in string/character literal at character 'u'
-}

-- >>> isLetter '蔡' && isLetter 'λ'
-- True

-- >>> parse parseJSON "\"fa λ An\""
-- [("fa \955 An","")]


-- token :: Parser a -> Parser a
-- token p = do space
--              v <- p
--              space
--              return v

-- data JSON = JNull
--           | JNumber Int
--           | JString String
--           | JBool Bool
--           | JArray [JSON]
--           | JObject [(String, JSON)]

parseJSON :: Parser JSON
parseJSON =
  token parseJNull
    <|> token parseJNumber
    <|> token parseJBool
    <|> token parseJArray
    <|> token parseJObject
    <|> token parseJString

{- >>> parse parseJSON "\"{"foo": {"2":2,"1":1}, "bar": [1.,2.4,3e3,4.5E2]}\""
Variable not in scope: foo
Variable not in scope: bar
-}
