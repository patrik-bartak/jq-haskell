{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
-- DictIndexing Req
compile (DictIndexing _ Req _) JNull = Left "Cannot DictIndex null"
compile (DictIndexing _ Req _) (JNumber _) = Left "Cannot DictIndex number"
compile (DictIndexing _ Req _) (JString _) = Left "Cannot DictIndex string"
compile (DictIndexing _ Req _) (JBool _) = Left "Cannot DictIndex bool"
compile (DictIndexing _ Req _) (JArray _) = Left "Cannot DictIndex array"
-- DictIndexing Opt
compile (DictIndexing _ Opt _) JNull = return []
compile (DictIndexing _ Opt _) (JNumber _) = return []
compile (DictIndexing _ Opt _) (JString _) = return []
compile (DictIndexing _ Opt _) (JBool _) = return []
compile (DictIndexing _ Opt _) (JArray _) = return []
-- DictIndexing All
compile (DictIndexing _ _ _) (JObject []) = return [JNull]
compile (DictIndexing idxType optio fieldIdx) (JObject ((field, value):xs))
  | fieldIdx == field   = return [value]
  | otherwise           = compile (DictIndexing idxType optio fieldIdx) (JObject xs)
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
-- ArrayIndexing All
compile (ArrayIndexing _ _) (JArray []) = return [JNull]
compile (ArrayIndexing optio idx) (JArray (x:xs))
  | idx == 0     = return [x]
  | idx > 0     = compile (ArrayIndexing optio (idx - 1)) (JArray xs)
  | otherwise    = compile (ArrayIndexing optio (-(idx + 1))) (JArray (reverse xs))
-- ArraySlice Req
compile (ArraySlice Req _ _) JNull = Left "Cannot ArraySlice null"
compile (ArraySlice Req _ _) (JNumber _) = Left "Cannot ArraySlice number"
compile (ArraySlice Req _ _) (JString _) = Left "Cannot ArraySlice string"
compile (ArraySlice Req _ _) (JBool _) = Left "Cannot ArraySlice bool"
compile (ArraySlice Req _ _) (JObject _) = Left "Cannot ArraySlice object"
-- ArraySlice Opt
compile (ArraySlice Opt _ _) JNull = return []
compile (ArraySlice Opt _ _) (JNumber _) = return []
compile (ArraySlice Opt _ _) (JString _) = return []
compile (ArraySlice Opt _ _) (JBool _) = return []
compile (ArraySlice Opt _ _) (JObject _) = return []
-- ArraySlice All
compile (ArraySlice _ _ _) (JArray []) = return [JArray []]
compile (ArraySlice _ lo hi) (JArray xs)
  | hi <= lo = return [JArray []]
  | otherwise = return [JArray (take hi (drop lo xs))]
-- Pipe
compile (Pipe f1 f2) inp = do
  res1 <- compile f1 inp                          -- :: Either String [JSON]
  let list_of_either = [compile f2 r | r <- res1] -- :: [Either String [JSON]]
  let sequenced_eithers = sequence list_of_either -- :: Either String [[JSON]]
  fmap concat sequenced_eithers                   -- :: Either String [JSON]
-- Comma
compile (Comma f1 f2) inp = do
  res1 <- compile f1 inp
  res2 <- compile f2 inp
  return (res1 ++ res2)
-- Paren
compile (Paren filt) inp = compile filt inp
-- Identity
compile Identity inp = return [inp]
-- RecDesc
compile RecDesc (JArray []) = return []
compile RecDesc (JArray jsons) = do
  -- deeper <- sequence (fmap (compile RecDesc) jsons)
  deeper <- mapM (compile RecDesc) jsons
  return (JArray jsons : concat deeper)
compile RecDesc (JObject []) = return []
compile RecDesc (JObject kvpairs) = do
  -- deeper <- sequence (fmap (compile RecDesc) (map snd kvpairs))
  deeper <- mapM (compile RecDesc . snd) kvpairs
  return (JObject kvpairs : concat deeper)
compile RecDesc inp = return [inp]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
-- run p j = p j
run p = p
