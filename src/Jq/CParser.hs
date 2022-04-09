module Jq.CParser where

import Jq.Filters
import Jq.JParser (parseJObjectField)
import Parsing.Parsing

parseIndexPeriod :: Parser Char
parseIndexPeriod = token (char '.')

parseSliceSep :: Parser Char
parseSliceSep = token (char ':')

parseOptional :: Parser Char
parseOptional = token (char '?')

parseCommaSep :: Parser Char
parseCommaSep = token (char ',')

parsePipeSep :: Parser Char
parsePipeSep = token (char '|')

parseIndexingOpen :: Parser Char
parseIndexingOpen = token (char '[')

parseIndexingClose :: Parser Char
parseIndexingClose = token (char ']')

parseParenOpen :: Parser Char
parseParenOpen = token (char '(')

parseParenClose :: Parser Char
parseParenClose = token (char ')')

parseIdenOpt :: Parser Filter
parseIdenOpt = do
  -- _ <- parseIndexPeriod
  identStr <- parseJObjectField <|> ident
  _ <- parseOptional
  return (DictIndexing Iden Opt identStr)

parseIdenReq :: Parser Filter
parseIdenReq = do
  -- _ <- parseIndexPeriod
  identStr <- parseJObjectField <|> ident
  return (DictIndexing Iden Req identStr)

parseGenOpt :: Parser Filter
parseGenOpt = do
  _ <- parseIndexingOpen
  identStr <- parseJObjectField
  _ <- parseIndexingClose
  _ <- parseOptional
  return (DictIndexing Gen Opt identStr)

parseGenReq :: Parser Filter
parseGenReq = do
  _ <- parseIndexingOpen
  identStr <- parseJObjectField
  _ <- parseIndexingClose
  return (DictIndexing Gen Req identStr)

parseArrayOpt :: Parser Filter
parseArrayOpt = do
  _ <- parseIndexingOpen
  idx <- int
  _ <- parseIndexingClose
  _ <- parseOptional
  return (ArrayIndexing Opt idx)

parseArrayReq :: Parser Filter
parseArrayReq = do
  _ <- parseIndexingOpen
  idx <- int
  _ <- parseIndexingClose
  return (ArrayIndexing Req idx)

parseInnerSlice :: Parser (Int, Int)
parseInnerSlice = do
  lo <- int
  _ <- parseSliceSep
  hi <- int
  return (lo, hi)

parseSliceOpt :: Parser Filter
parseSliceOpt = do
  _ <- parseIndexingOpen
  (lo, hi) <- parseInnerSlice
  _ <- parseIndexingClose
  _ <- parseOptional
  return (ArraySlice Opt lo hi)

parseSliceReq :: Parser Filter
parseSliceReq = do
  _ <- parseIndexingOpen
  (lo, hi) <- parseInnerSlice
  _ <- parseIndexingClose
  return (ArraySlice Req lo hi)

-- parseInnerIter :: Parser [Int]
-- parseInnerIter = do
--       idx <- int
--       innerJsons <- many parseInnerJsons
--       return (JArray (singleInnerJson:innerJsons))
--             where parseInnerJsons = do
--                               _ <- parseMultiValueSeperator
--                               parseJSON

-- parseIterOpt :: Parser Filter
-- parseIterOpt = do
--   _ <- parseIndexingOpen
--   idxs <- parseInnerIter
--   _ <- parseIndexingClose
--   _ <- parseOptional
--   return (Iter Opt idxs)

-- parseIterReq :: Parser Filter
-- parseIterReq = do
--   _ <- parseIndexingOpen
--   idxs <- parseInnerIter
--   _ <- parseIndexingClose
--   return (Iter Req idxs)

parseDictIndexing :: Parser Filter
parseDictIndexing = do
  -- _ <- parseIndexPeriod
  fieldIdx <-
    parseIdentity <|> parseGenOpt <|> parseGenReq <|> parseIdenOpt <|> parseIdenReq
      <|> parseArrayOpt
      <|> parseArrayReq
      <|> parseSliceOpt
      <|> parseSliceReq
  -- <|> parseIterOpt <|> parseIterReq
  -- <|> parseArrayIterOpt <|> parseArrayIterReq
  -- <|> parseDictIterOpt <|> parseDictIterReq
  nestedIdxs <- many parseDictIndexing
  return (foldr (flip Pipe) fieldIdx nestedIdxs)

-- >>> parse parseFilter ".foo.[5].[\"bar\"].[4:5]"
-- [(. | foo | . | [5] | . | ["bar"] | . | [4:5],"")]
-- >>> parse parseFilter ".foo.bar"
-- [(. | foo | . | bar,"")]

-- -- DictIndexing
-- parseDictIndexing :: Parser Filter
-- parseDictIndexing = do
--   _ <- parseIndexPeriod
--   dictIdx <- ident
--   nextIdxFilt <- many parseIndexing
--   -- Apply this to others so that I can recursively index
--   return (foldr Pipe Identity (DictIndexing dictIdx:nextIdxFilt))

-- parseDictOptIndexing :: Parser Filter
-- parseDictOptIndexing = do
--   idxfilter <- parseDictIndexing
--   _ <- parseOptional
--   let identStr = case idxfilter of
--         DictIndexing s -> s
--         _ -> ""
--   return (DictOptIndexing identStr)

-- -- DictGenIndexing
-- parseDictGenIndexing :: Parser Filter
-- parseDictGenIndexing = do
--   _ <- parseIndexPeriod
--   _ <- parseIndexingOpen
--   identStr <- parseJObjectField
--   _ <- parseIndexingClose
--   return (DictGenIndexing identStr)

-- parseDictGenOptIndexing :: Parser Filter
-- parseDictGenOptIndexing = do
--   idxfilter <- parseDictGenIndexing
--   _ <- parseOptional
--   let identStr = case idxfilter of
--         DictIndexing s -> s
--         _ -> ""
--   return (DictGenOptIndexing identStr)

-- Comma
parseComma :: Parser Filter
parseComma = do
  filt1 <- parsePipe <|> parseFilterNotInfix
  _ <- parseCommaSep
  filt2 <- parseFilter
  return (Comma filt1 filt2)

-- Pipe
parsePipe :: Parser Filter
parsePipe = do
  filt1 <- parseComma <|> parseFilterNotInfix
  _ <- parsePipeSep
  filt2 <- parseFilter
  return (Pipe filt1 filt2)

-- Paren
parseParen :: Parser Filter
parseParen = do
  _ <- parseParenOpen
  filt <- parseFilter
  _ <- parseParenClose
  return (Paren filt)

-- Identity
parseIdentity :: Parser Filter
parseIdentity = do
  _ <- parseIndexPeriod
  return Identity

-- RecDesc
parseRecDesc :: Parser Filter
parseRecDesc = do
  _ <- parseIndexPeriod
  _ <- parseIndexPeriod
  return RecDesc

-- JSON filter
parseJSONFilter :: Parser Filter
parseJSONFilter = undefined

-- Main parsing function
parseFilter :: Parser Filter
parseFilter =
  parseBinaryFilters
    <|> parseFilterNotInfix
    <|> parseJSONFilter

parseFilterNotInfix :: Parser Filter
parseFilterNotInfix =
  parseRecDesc <|> parseIndexing
    -- <|> parseArrayOptSlice <|> parseArraySlice
    -- <|> parseArrayOptIter <|> parseArrayIter
    -- <|> parseDictOptIter <|> parseDictIter
    <|> parseParen
    <|> parseIdentity

parseIndexing :: Parser Filter
parseIndexing = parseDictIndexing

-- <|> parseArrayIndexing

parseBinaryFilters :: Parser Filter
parseBinaryFilters =
  parsePipe
    <|> parseComma

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
