{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}

module Jq.Compiler where

import Jq.Filters
import Jq.Json

type JProgram a = JSON -> Either String a

sliceList :: [a] -> Int -> Int -> [a]
sliceList xs lo hi = take (hi - lo) (drop lo xs)

normalizeIndices :: (Int, Int) -> [a] -> (Int, Int)
normalizeIndices (lo, hi) xs = (normalizeIndex lo xs, normalizeIndex hi xs)

normalizeIndex :: Int -> [a] -> Int
normalizeIndex idx xs
  | idx < 0 = idx + len
  | otherwise = idx
  where
    len = length xs

compile :: Filter -> JProgram [JSON]
-- DictIdenIndexing Req
compile (DictIdenIndexing Req _) JNull = Left "Cannot DictIndex null"
compile (DictIdenIndexing Req _) (JNumber _) = Left "Cannot DictIndex number"
compile (DictIdenIndexing Req _) (JString _) = Left "Cannot DictIndex string"
compile (DictIdenIndexing Req _) (JBool _) = Left "Cannot DictIndex bool"
compile (DictIdenIndexing Req _) (JArray _) = Left "Cannot DictIndex array"
-- DictIdenIndexing Opt
compile (DictIdenIndexing Opt _) JNull = return []
compile (DictIdenIndexing Opt _) (JNumber _) = return []
compile (DictIdenIndexing Opt _) (JString _) = return []
compile (DictIdenIndexing Opt _) (JBool _) = return []
compile (DictIdenIndexing Opt _) (JArray _) = return []
-- DictIdenIndexing All
compile (DictIdenIndexing _ _) (JObject []) = return [JNull]
compile (DictIdenIndexing optio fieldIdx) (JObject ((field, value) : xs))
  | fieldIdx == field = return [value]
  | otherwise = compile (DictIdenIndexing optio fieldIdx) (JObject xs)
-- DictGenIndexing Req
compile (DictGenIndexing Req _) JNull = Left "Cannot DictIndex null"
compile (DictGenIndexing Req _) (JNumber _) = Left "Cannot DictIndex number"
compile (DictGenIndexing Req _) (JString _) = Left "Cannot DictIndex string"
compile (DictGenIndexing Req _) (JBool _) = Left "Cannot DictIndex bool"
compile (DictGenIndexing Req _) (JArray _) = Left "Cannot DictIndex array"
-- DictGenIndexing Opt
compile (DictGenIndexing Opt _) JNull = return []
compile (DictGenIndexing Opt _) (JNumber _) = return []
compile (DictGenIndexing Opt _) (JString _) = return []
compile (DictGenIndexing Opt _) (JBool _) = return []
compile (DictGenIndexing Opt _) (JArray _) = return []
-- DictGenIndexing All
compile (DictGenIndexing _ _) (JObject []) = return [JNull]
compile (DictGenIndexing optio fieldIdxs) (JObject kvPairs) =
  fmap concat (sequence indexedList)
    where
      indexedList = map f fieldIdxs
      f = \fieldIdx -> compile (DictIdenIndexing optio fieldIdx) (JObject kvPairs)
-- ArrayIndexing Req
compile (ArrayIndexing Req _) JNull = Left "Cannot ArrayIndex null"
compile (ArrayIndexing Req _) (JNumber _) = Left "Cannot ArrayIndex number"
compile (ArrayIndexing Req _) (JString _) = Left "Cannot ArrayIndex string"
compile (ArrayIndexing Req _) (JBool _) = Left "Cannot ArrayIndex bool"
compile (ArrayIndexing Req _) (JObject _) = Left "Cannot ArrayIndex object"
-- ArrayIndexing Opt
compile (ArrayIndexing Opt _) JNull = return []
compile (ArrayIndexing Opt _) (JNumber _) = return []
compile (ArrayIndexing Opt _) (JString _) = return []
compile (ArrayIndexing Opt _) (JBool _) = return []
compile (ArrayIndexing Opt _) (JObject _) = return []
-- ArrayIndexing String -> turns out that's not a thing in Jq :(
-- compile (ArrayIndexing _ _) (JString []) = return [JNull]
-- compile (ArrayIndexing optio idx) (JString (x:xs))
--   | idx == 0     = return [JString [x]]
--   | idx > 0     = compile (ArrayIndexing optio (idx - 1)) (JString xs)
--   | otherwise    = compile (ArrayIndexing optio (-(idx + 1))) (JString (reverse xs))
-- ArrayIndexing All
compile (ArrayIndexing _ _) (JArray []) = return [JNull]
compile (ArrayIndexing optio idx) (JArray (x : xs))
  | norm_idx == 0 = return [x]
  | otherwise = compile (ArrayIndexing optio (norm_idx - 1)) (JArray xs)
  where
    norm_idx = normalizeIndex idx (x : xs)
-- ArraySlice Req
compile (ArraySlice Req _ _) JNull = Left "Cannot ArraySlice null"
compile (ArraySlice Req _ _) (JNumber _) = Left "Cannot ArraySlice number"
compile (ArraySlice Req _ _) (JBool _) = Left "Cannot ArraySlice bool"
compile (ArraySlice Req _ _) (JObject _) = Left "Cannot ArraySlice object"
-- ArraySlice Opt
compile (ArraySlice Opt _ _) JNull = return []
compile (ArraySlice Opt _ _) (JNumber _) = return []
compile (ArraySlice Opt _ _) (JBool _) = return []
compile (ArraySlice Opt _ _) (JObject _) = return []
-- ArraySlice String
compile (ArraySlice _ _ _) (JString []) = return [JString []]
compile (ArraySlice _ lo hi) (JString xs)
  | norm_hi <= norm_lo = return [JString []]
  | otherwise = return [JString (sliceList xs norm_lo norm_hi)]
  where
    (norm_lo, norm_hi) = normalizeIndices (lo, hi) xs
-- ArraySlice All
compile (ArraySlice _ _ _) (JArray []) = return [JArray []]
compile (ArraySlice _ lo hi) (JArray xs)
  | norm_hi <= norm_lo = return [JArray []]
  | otherwise = return [JArray (sliceList xs norm_lo norm_hi)]
  where
    (norm_lo, norm_hi) = normalizeIndices (lo, hi) xs
-- Iterator Req
compile (Iterator Req _) JNull = Left "Cannot Iterate null"
compile (Iterator Req _) (JNumber _) = Left "Cannot Iterate number"
compile (Iterator Req _) (JString _) = Left "Cannot Iterate string"
compile (Iterator Req _) (JBool _) = Left "Cannot Iterate bool"
-- Iterator Opt
compile (Iterator Opt _) JNull = return []
compile (Iterator Opt _) (JNumber _) = return []
compile (Iterator Opt _) (JString _) = return []
compile (Iterator Opt _) (JBool _) = return []
-- Iterator Dict Empty
compile (Iterator _ []) (JObject kvPairs) = return values
  where
    values = map snd kvPairs
-- Iterator Array Empty
compile (Iterator _ []) (JArray values) = return values
-- Iterator Dict
compile (Iterator optio idxs) (JObject kvPairs) = 
  fmap concat (sequence indexedList)
    where
      values = map snd kvPairs
      indexedList = map f idxs
      f = \idx -> compile (ArrayIndexing optio idx) (JArray values)
-- Iterator Array
compile (Iterator optio idxs) (JArray values) = 
  fmap concat (sequence indexedList)
    where
      indexedList = map f idxs
      f = \idx -> compile (ArrayIndexing optio idx) (JArray values)
-- Pipe
compile (Pipe f1 f2) inp = do
  res1 <- compile f1 inp -- :: Either String [JSON]
  let list_of_either = [compile f2 r | r <- res1] -- :: [Either String [JSON]]
  let sequenced_eithers = sequence list_of_either -- :: Either String [[JSON]]
  fmap concat sequenced_eithers -- :: Either String [JSON]
  -- Comma
compile (Comma f1 f2) inp = do
  res1 <- compile f1 inp
  res2 <- compile f2 inp
  return (res1 ++ res2)
-- Paren
compile (Paren filt) inp = compile filt inp
-- Identity
compile Identity inp = return [inp]
-- RecDesc JArray
compile RecDesc (JArray []) = return []
compile RecDesc (JArray jsons) = do
  -- deeper <- sequence (fmap (compile RecDesc) jsons)
  deeper <- mapM (compile RecDesc) jsons
  return (JArray jsons : concat deeper)
-- RecDesc JObject
compile RecDesc (JObject []) = return []
compile RecDesc (JObject kvpairs) = do
  -- deeper <- sequence (fmap (compile RecDesc) (map snd kvpairs))
  deeper <- mapM (compile RecDesc . snd) kvpairs
  return (JObject kvpairs : concat deeper)
-- RecDesc other JSONs
compile RecDesc inp = return [inp]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
-- run p j = p j
run p = p
