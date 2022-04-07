module Jq.JParser where

import Parsing.Parsing
import Jq.Json

parseJNull :: Parser JSON
parseJNull = do
      _ <- string "null"
      return JNull

parseJNumber :: Parser JSON
-- parseJNumber = do
--       num <- int
--       return (JNumber num)
parseJNumber = do JNumber <$> int

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
      return (JArray (singleInnerJson:innerJsons))
            where parseInnerJsons = do
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
      return (
        case json of
          JString str -> str
          _ -> ""
        )

parseMultiValueSeperator :: Parser Char
parseMultiValueSeperator = token (char ',')

parseJObjectKeyValPair :: Parser (String, JSON)
parseJObjectKeyValPair = token (do
      key <- parseJObjectField
      _ <- parseJObjectMapChar
      singleInnerJson <- parseJSON
      return (key, singleInnerJson))

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
      return (JObject ((key,singleInnerJson):innerJsons))
            where parseJObjectInnerJsons = do
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

innerJString :: Parser String
innerJString = some (alphanum <|> char '_')
-- innerJString = unicode
innerJStringEscaped :: Parser String
innerJStringEscaped = string "\\\"" <|> string "\\\\" <|> string "\n"

parseJString :: Parser JSON
parseJString = do
      _ <- char '"'
      -- looking for \" or ident
      list_of_strs <- many (innerJStringEscaped <|> innerJString)
      _ <- char '"'
      return (JString (concat list_of_strs))
-- >>> parse parseJSON "\"faasd\\\"w234_lse\""
-- [("faasd\"w234_lse","")]

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
parseJSON = token parseJNull
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
