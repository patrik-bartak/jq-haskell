{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Jq.CParser where

import Jq.Filters
import Jq.JParser (parseJObjectField, parseMultiValueSeperator, parseJNull, parseJNumber, parseJBool, parseJString)
import Parsing.Parsing
import Data.Map hiding (foldr, filter, empty)
import Prelude hiding (lookup)
import Data.List (isInfixOf)


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

parseInnerSlice :: Parser (Filter, Filter)
parseInnerSlice = do
  lo <- parseFilter
  _ <- parseSliceSep
  hi <- parseFilter
  return (lo, hi)

parseSlice :: Parser Filter
parseSlice = do
  _ <- parseIndexingOpen
  (lo, hi) <- parseInnerSlice
  _ <- parseIndexingClose
  opt <- parseOrElse parseOptional '!'
  return (if opt == '?' then ArraySlice Opt lo hi else ArraySlice Req lo hi)

-- >>> parse parseFilter ".[1:2]"
-- [(([1:2]|.),"")]

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
  return (Paren (Prelude.foldr Pipe Identity (firstIdx:fieldIdxs)))

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
parseFilter = parseInfixLevel 0

-- Recursive descent infix precedence
parseInfixLevel :: Int -> Parser Filter
parseInfixLevel n = case lookup n infixPrecedenceMap of
  Just parser -> parser <|> parseInfixLevel (n+1)
  Nothing -> parseFilterNotInfix

-- Precedence associated to each infix operator
infixPrecedenceMap :: Map Int (Parser Filter)
infixPrecedenceMap = fromList [
    (0, parsePipe),
    (1, parseComma),
    (2, parseEquals),
    (3, parseNotEquals),
    (4, parseLogicalOr),
    (5, parseLogicalAnd),
    (6, parseLogicalNot)
  ]

parseFilterNotInfix :: Parser Filter
parseFilterNotInfix = parseRecDesc
                  <|> parseParen
                  <|> parseIndexing
                  <|> parseIdentity
                  <|> parseJSONFilters

parseJSONFilters :: Parser Filter
parseJSONFilters =
        token parseJNullFilter
    <|> token parseJNumberFilter
    <|> token parseJBoolFilter
    <|> token parseJArrayFilter
    -- <|> token parseJObjectFilter
    <|> token parseJStringFilter

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e

-- Primitive value constructors
parseJNullFilter :: Parser Filter
parseJNullFilter = JNullFilter <$> parseJNull
parseJNumberFilter :: Parser Filter
parseJNumberFilter = JNumberFilter <$> parseJNumber
parseJBoolFilter :: Parser Filter
parseJBoolFilter = JBoolFilter <$> parseJBool
parseJStringFilter :: Parser Filter
parseJStringFilter = JStringFilter <$> parseJString

-- JArray value constructor
parseJArrayFilter :: Parser Filter
parseJArrayFilter = parseJArrayFilterEmpty <|> parseJArrayFilterSingleton <|> parseJArrayFilterMultiple

parseJArrayFilterOpen :: Parser Char
parseJArrayFilterOpen = char '['

parseJArrayFilterClose :: Parser Char
parseJArrayFilterClose = char ']'

parseJArrayFilterEmpty :: Parser Filter
parseJArrayFilterEmpty = do
  _ <- parseJArrayFilterOpen
  _ <- parseJArrayFilterClose
  return (JArrayFilter [])

parseJArrayFilterSingleton :: Parser Filter
parseJArrayFilterSingleton = do
  _ <- parseJArrayFilterOpen
  singleInnerJson <- parseFilter
  _ <- parseJArrayFilterClose
  return (JArrayFilter [singleInnerJson])

parseJArrayFilterMultiple :: Parser Filter
parseJArrayFilterMultiple = do
  _ <- parseJArrayFilterOpen
  singleInnerJson <- parseFilter
  innerJsons <- many parseInnerJsons
  _ <- parseJArrayFilterClose
  return (JArrayFilter (singleInnerJson : innerJsons))
  where
    parseInnerJsons = do
      _ <- parseMultiValueSeperator
      parseFilter

-- JObject value constructor

-- Infix operators & their precedence
-- >>> parse parseFilter ".|.,.|."
-- [((.|((.,.)|.)),"")]

-- should be (.|(.,.)|.))

-- >>> parse parseFilter ".,.|.,."
-- [(((.,.)|(.,.)),"")]

-- should be (.,.)|(.,.)

-- Speed optimization for infix operators
parseIfContains :: String -> Parser Filter -> Parser Filter
parseIfContains op p = P (\input -> if op `isInfixOf` input then parse p input else empty)

-- Pipe - 0
parsePipe :: Parser Filter
parsePipe = do
  let op = "|"
  let opPrec = 0
  filt1 <- parseIfContains op (parseInfixLevel (opPrec + 1))
  _ <- token (string op)
  filt2 <- parseInfixLevel opPrec
  return (Paren (Pipe filt1 filt2))

-- Comma - 1
parseComma :: Parser Filter
parseComma = do
  let op = ","
  let opPrec = 1
  filt1 <- parseIfContains op (parseInfixLevel (opPrec + 1))
  _ <- token (string op)
  filt2 <- parseInfixLevel opPrec
  return (Paren (Comma filt1 filt2))

-- Equals - 2
parseEquals :: Parser Filter
parseEquals = do
  let op = "=="
  let opPrec = 2
  left <- parseIfContains op (parseInfixLevel (opPrec + 1))
  _ <- token (string op)
  right <- parseInfixLevel opPrec
  return (Paren (Equals left right))

-- NotEquals - 3
parseNotEquals :: Parser Filter
parseNotEquals = do
  let op = "!="
  let opPrec = 3
  left <- parseIfContains op (parseInfixLevel (opPrec + 1))
  _ <- token (string op)
  right <- parseInfixLevel opPrec
  return (Paren (NotEquals left right))

-- LogicalOr - 4
parseLogicalOr :: Parser Filter
parseLogicalOr = do
  let op = "or"
  let opPrec = 4
  left <- parseIfContains op (parseInfixLevel (opPrec + 1))
  _ <- token (string op)
  right <- parseInfixLevel opPrec
  return (Paren (LogicalOr left right))

-- LogicalAnd - 5
parseLogicalAnd :: Parser Filter
parseLogicalAnd = do
  let op = "and"
  let opPrec = 5
  left <- parseIfContains op (parseInfixLevel (opPrec + 1))
  _ <- token (string op)
  right <- parseInfixLevel opPrec
  return (Paren (LogicalAnd left right))

-- LogicalAnd - 6
parseLogicalNot :: Parser Filter
parseLogicalNot = do
  _ <- token (string "not")
  return (Paren LogicalNot)

-- >>> parse parseFilter "true and true or true and true"
-- [(((true and true) or (true and true)),"")]

-- should be ((true and true) or (true and true))




-- For some reason I decided to spend time implementing Dijkstra's shunting yard algorithm?

-- >>> parse parseFilter "1==1!=1,1|1==1!=1"
-- [((((1 == (1 != 1)),1)|(1 == (1 != 1))),"")]

-- operators :: [Filter]
-- operators = keys precedence

-- precedence :: Map Filter Int
-- precedence = fromList [
--   (NotEquals Identity Identity, 2), 
--   (Equals Identity Identity, 1)
--   ]

-- yard :: String -> [Char] -> String
-- yard [] stack = stack
-- yard (nextToken:tokens) stack
--   | nextToken `elem` operators = 
--       let greater = getGreaterPrecedence nextToken stack
--           remaining = getRestOfStack nextToken stack
--           in greater ++ yard tokens (nextToken:remaining)
--   | otherwise = nextToken : yard tokens stack

-- getGreaterPrecedence :: Char -> [Char] -> [Char]
-- getGreaterPrecedence _ [] = []
-- getGreaterPrecedence toFind (top:stack)
--   | Data.Map.lookup top precedence > myPrec = top : getGreaterPrecedence toFind stack
--   | otherwise                               = []
--       where myPrec = Data.Map.lookup toFind precedence

-- getRestOfStack :: Char -> [Char] -> [Char]
-- getRestOfStack _ [] = []
-- getRestOfStack toFind (top:stack)
--   | Data.Map.lookup top precedence > myPrec = getRestOfStack toFind stack
--   | otherwise                               = top:stack
--       where myPrec = Data.Map.lookup toFind precedence

-- st :: [Char]
-- st = "**++**"
-- mychar :: Char
-- mychar = '+'

-- >>> (getGreaterPrecedence mychar st, getRestOfStack mychar st)
-- ("**","++**")

-- >>> yard "2+4*1+2" []
-- "241*2++"

-- should be (.,.)|(.,.)

-- postfixToInfix :: [String] -> [String] -> [String]
-- postfixToInfix [] stack = stack
-- postfixToInfix (nextToken:tokens) stack
--   | nextToken `elem` operators = case stack of
--                           fst:snd:rest -> postfixToInfix tokens (("(" ++ fst ++ nextToken ++ snd ++ ")"):rest)
--   | otherwise = postfixToInfix tokens (nextToken:stack)

-- deconcat :: [a] -> [[a]]
-- deconcat [] = []
-- deconcat (s:str) = [s] : deconcat str

-- >>> (deconcat "241*2++")
-- ["2","4","1","*","2","+","+"]

-- >>> concat (postfixToInfix (deconcat "241*2++") [])
-- "((2+(1*4))+2)"
