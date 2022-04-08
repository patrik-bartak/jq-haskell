module Jq.JParser where

import Data.Char
import GHC.Unicode
import Jq.Json
import Parsing.Parsing

parseJNull :: Parser JSON
parseJNull = do
  _ <- string "null"
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

innerJString :: Parser String
innerJString = some (alphanum <|> char '_' <|> char '.' <|> char ' ' <|> sat isSymbol <|> sat isSeparator)
--  <|> sat isPrint
-- innerJString = some (sat (const True))
innerJStringEscaped :: Parser String
innerJStringEscaped = string "\\\"" <|> string "\\" <|> string "\n" <|> string "\t" <|> string "\r" <|> string "\f" <|> string "\v"

parseJString :: Parser JSON
parseJString = do
  _ <- char '"'
  -- looking for \" or ident
  list_of_strs <- many (innerJStringEscaped <|> innerJString)
  _ <- char '"'
  return (JString (concat list_of_strs))

{- >>> parse parseJSON "[\"\u2600\""]"
lexical error in string/character literal at character 'u'
-}
{- >>> parse parseJSON "[\"☀\"]"
[]
-}

-- >>> isLetter '蔡' && isLetter 'λ'
-- True
-- >>> parse parseJSON "\"fa λ An\""
-- []
-- >>> parse parseJSON "\"faAn\""
-- []
-- >>> parse parseJSON "\"\""
-- []

-- Parser JSON :: P (String -> [(JSON,String)])
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

{- >>> parse parseJSON "{\"foo\": 42, \"bar\": [false]}"
[({
  "foo": 42,
  "bar": [
    false
  ]
},"")]
-}

{- >>> parse parseJSON "{\"kokot\":\"Matej\"}"
[({
  "kokot": "Matej"
},"")]
-}
