module Jq.CParser where

import Jq.Filters
import Jq.JParser (parseJObjectField, parseMultiValueSeperator)
import Parsing.Parsing
import Data.Map


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

-- Either parses using the provided parser, or returns the alternate value if that fails
parseOrElse :: Parser a -> a -> Parser a
parseOrElse p alt = p <|> return alt

parseIdenNoDot :: Parser Filter
parseIdenNoDot = do
  identStr <- parseJObjectField <|> ident
  opt <- parseOrElse parseOptional '!'
  return (if opt == '?' then DictIdenIndexing Opt identStr else DictIdenIndexing Req identStr)

parseIdenWithDot :: Parser Filter
parseIdenWithDot = do
  _ <- parseIndexPeriod
  identStr <- parseJObjectField <|> ident
  opt <- parseOrElse parseOptional '!'
  return (if opt == '?' then DictIdenIndexing Opt identStr else DictIdenIndexing Req identStr)

parseArrayOpt :: Parser Filter
parseArrayOpt = do
  _ <- parseIndexingOpen
  idx <- int
  _ <- parseIndexingClose
  opt <- parseOrElse parseOptional '!'
  return (if opt == '?' then ArrayIndexing Opt idx else ArrayIndexing Req idx)

parseInnerSlice :: Parser (Int, Int)
parseInnerSlice = do
  lo <- int
  _ <- parseSliceSep
  hi <- int
  return (lo, hi)

parseSlice :: Parser Filter
parseSlice = do
  _ <- parseIndexingOpen
  (lo, hi) <- parseInnerSlice
  _ <- parseIndexingClose
  opt <- parseOrElse parseOptional '!'
  return (if opt == '?' then ArraySlice Opt lo hi else ArraySlice Req lo hi)

-- Identity
parseIdentity :: Parser Filter
parseIdentity = do
  _ <- parseIndexPeriod
  return Identity

parseEmptyIterator :: Parser Filter
parseEmptyIterator = do
  _ <- parseIndexingOpen
  _ <- parseIndexingClose
  opt <- parseOrElse parseOptional '!'
  return (if opt == '?' then Iterator Opt [] else Iterator Req [])

parseNonEmptyIterator :: Parser Filter
parseNonEmptyIterator = do
  _ <- parseIndexingOpen
  idx <- int
  idxs <- many otherInnerValues
  _ <- parseIndexingClose
  opt <- parseOrElse parseOptional '!'
  return (if opt == '?' then Iterator Opt (idx:idxs) else Iterator Req (idx:idxs))
    where otherInnerValues = do
              _ <- parseMultiValueSeperator
              int

parseDictGenIndexing :: Parser Filter
parseDictGenIndexing = do
  _ <- parseIndexingOpen
  field <- parseJObjectField
  fields <- many otherInnerValues
  _ <- parseIndexingClose
  opt <- parseOrElse parseOptional '!'
  return (if opt == '?' then DictGenIndexing Opt (field:fields) else DictGenIndexing Req (field:fields))
    where otherInnerValues = do
              _ <- parseMultiValueSeperator
              parseJObjectField

-- >>> parse parseFilter ".[\"asd\",\"asd\"]?.foo?[0]?"
-- [(["asd","asd"]?|foo?|[0]?|.,"")]

parseIndexing :: Parser Filter
parseIndexing = do
  _ <- parseIndexPeriod
  firstIdx <- parseFirstIndexType
  fieldIdxs <- many parseSubsequentIndexTypes
  return (Prelude.foldr Pipe Identity (firstIdx:fieldIdxs))

parseFirstIndexType :: Parser Filter
parseFirstIndexType =
          parseEmptyIterator
      <|> parseNonEmptyIterator
      <|> parseDictGenIndexing
      <|> parseIdenNoDot
      <|> parseArrayOpt
      <|> parseSlice

parseSubsequentIndexTypes :: Parser Filter
parseSubsequentIndexTypes =
          parseEmptyIterator
      <|> parseNonEmptyIterator
      <|> parseDictGenIndexing
      <|> parseIdenWithDot
      <|> parseArrayOpt
      <|> parseSlice

-- >>> parse parseFilter ".foo[5][\"bar\"][4:5]"
-- [(foo|[5]|["bar"]|[4:5]|.,"")]
-- >>> parse parseFilter ".foo.bar"
-- [(foo|bar|.,"")]

-- Comma
parseComma :: Parser Filter
parseComma = do
  filt1 <- parseFilterNotInfix -- all but pipe and comma
  _ <- parseCommaSep
  filt2 <- parseComma <|> parseFilterNotInfix -- all but pipe (pipe has lower precedence)
  return (Paren (Comma filt1 filt2))

-- Pipe
parsePipe :: Parser Filter
parsePipe = do
  filt1 <- parseComma <|> parseFilterNotInfix -- all but pipe (prevent infinite loop)
  _ <- parsePipeSep
  filt2 <- parseFilter
  return (Paren (Pipe filt1 filt2))

-- >>> parse parseFilter ".|.,.|."
-- [((.|((.,.)|.)),"")]

-- should be (.|(.,.)|.))

-- >>> parse parseFilter ".,.|.,."
-- [(((.,.)|(.,.)),"")]

-- should be (.,.)|(.,.)

-- Paren
parseParen :: Parser Filter
parseParen = do
  _ <- parseParenOpen
  filt <- parseFilter
  _ <- parseParenClose
  return (Paren filt)

-- RecDesc
parseRecDesc :: Parser Filter
parseRecDesc = do
  _ <- parseIndexPeriod
  _ <- parseIndexPeriod
  return RecDesc

-- JSON filter
-- parseJSONFilter :: Parser Filter
-- parseJSONFilter = undefined

-- Main parsing function
parseFilter :: Parser Filter
parseFilter = parseFiltersInfix
          <|> parseFilterNotInfix

parseFiltersInfix :: Parser Filter
parseFiltersInfix = parsePipe <|> parseComma

parseFilterNotInfix :: Parser Filter
parseFilterNotInfix = parseRecDesc
                  <|> parseParen
                  <|> parseIndexing
                  <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
