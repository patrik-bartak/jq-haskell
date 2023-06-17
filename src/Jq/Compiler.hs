{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-# HLINT ignore "Eta reduce" #-}

module Jq.Compiler where

import Jq.Filters
import Jq.Json
import Data.List (elemIndex)


type JProgram a = JSON -> Either String a

sliceList :: [a] -> Int -> Int -> [a]
sliceList xs lo hi = take (hi - lo) (drop lo xs)

normalizeIndices :: (Int, Int) -> [a] -> (Int, Int, Bool)
normalizeIndices (lo, hi) xs = (normLo, normHi, succLo && succHi)
  where (normLo, succLo) = normalizeIndex lo xs
        (normHi, succHi) = normalizeIndex hi xs

normalizeIndicesDub :: (Double, Double) -> [a] -> (Double, Double, Bool)
normalizeIndicesDub (lo, hi) xs = (normLo, normHi, succLo && succHi)
  where (normLo, succLo) = normalizeIndexDub lo xs
        (normHi, succHi) = normalizeIndexDub hi xs

normalizeIndex :: Int -> [a] -> (Int, Bool)
normalizeIndex idx xs
  | idx < 0 = if normalized >= 0 then (idx + len, True) else (idx + len, False)
  | otherwise = (idx, True)
  where
    len = length xs
    normalized = idx + len

normalizeIndexDub :: Double -> [a] -> (Double, Bool)
normalizeIndexDub idx xs
  | idx < 0 = if normalized >= 0 then (idx + fromIntegral len, True) else (idx + fromIntegral len, False)
  | otherwise = (idx, True)
  where
    len = length xs
    normalized = idx + fromIntegral len

-- >>> normalizeIndexDub (-7.0) [1,2,3]
-- (-4.0,False)

compile :: Filter -> JProgram [JSON]
compile Empty _ = return []
-- Length
compile Length JNull = Left "Cannot get Length of JNull"
compile Length (JNumber _) = Left "Cannot get Length of JNumber"
compile Length (JString str) = return [JNumber $ fromIntegral $ length str]
compile Length (JBool _) = Left "Cannot get Length of JBool"
compile Length (JArray arr) = return [JNumber $ fromIntegral $ length arr]
compile Length (JObject obj) = return [JNumber $ fromIntegral $ length obj]
-- ToNumber
compile ToNumber JNull = Left "Cannot call ToNumber on JNull"
compile ToNumber (JNumber num) = return [JNumber num]
compile ToNumber (JString str) = return [JNumber $ read str]
compile ToNumber (JBool bool) = return [JNumber $ fromIntegral $ fromEnum bool]
compile ToNumber (JArray arr) = Left "Cannot call ToNumber on JArray"
compile ToNumber (JObject obj) = Left "Cannot call ToNumber on JObject"
-- Any
compile Any JNull = Left "Cannot call Any on JNull"
compile Any (JNumber num) = Left "Cannot call Any on JNumber"
compile Any (JString str) = Left "Cannot call Any on JString"
compile Any (JBool bool) = Left "Cannot call Any on JBool"
compile Any (JArray arr) = return [JBool $ or $ (map getJsonTruthValue arr)]
compile Any (JObject obj) = Left "Cannot call Any on JObject"
-- All
compile All JNull = Left "Cannot call All on JNull"
compile All (JNumber num) = Left "Cannot call All on JNumber"
compile All (JString str) = Left "Cannot call All on JString"
compile All (JBool bool) = Left "Cannot call All on JBool"
compile All (JArray arr) = return [JBool $ and $ (map getJsonTruthValue arr)]
-- compile All (JArray arr) = return [JBool $ any (<this should be a function>) $ (map getJsonTruthValue arr)]
compile All (JObject obj) = Left "Cannot call All on JObject"
-- DictIdenIndexing Req
compile (DictIdenIndexing Req _) JNull = return [JNull]
compile (DictIdenIndexing Req _) (JNumber _) = Left "Cannot DictIndex number"
compile (DictIdenIndexing Req _) (JString _) = Left "Cannot DictIndex string"
compile (DictIdenIndexing Req _) (JBool _) = Left "Cannot DictIndex bool"
compile (DictIdenIndexing Req _) (JArray _) = Left "Cannot DictIndex array"
-- DictIdenIndexing Opt
compile (DictIdenIndexing Opt _) JNull = return [JNull]
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
compile (DictGenIndexing Req _) (JNumber _) = Left "Cannot DictIndex number"
compile (DictGenIndexing Req _) (JString _) = Left "Cannot DictIndex string"
compile (DictGenIndexing Req _) (JBool _) = Left "Cannot DictIndex bool"
compile (DictGenIndexing Req _) (JArray _) = Left "Cannot DictIndex array"
-- DictGenIndexing Opt
compile (DictGenIndexing Opt _) (JNumber _) = return []
compile (DictGenIndexing Opt _) (JString _) = return []
compile (DictGenIndexing Opt _) (JBool _) = return []
compile (DictGenIndexing Opt _) (JArray _) = return []
-- DictGenIndexing All
compile (DictGenIndexing _ _) (JObject []) = return [JNull]
compile (DictGenIndexing optio fieldIdxs) inp =
  fmap concat (sequence indexedList)
    where
      indexedList = map f fieldIdxs
      f = \fieldIdx -> compile (DictIdenIndexing optio fieldIdx) inp
-- ArrayIndexing Req
compile (ArrayIndexing Req _) (JNumber _) = Left "Cannot ArrayIndex number"
compile (ArrayIndexing Req _) (JString _) = Left "Cannot ArrayIndex string"
compile (ArrayIndexing Req _) (JBool _) = Left "Cannot ArrayIndex bool"
compile (ArrayIndexing Req _) (JObject _) = Left "Cannot ArrayIndex object"
-- ArrayIndexing Opt
compile (ArrayIndexing Opt _) (JNumber _) = return []
compile (ArrayIndexing Opt _) (JString _) = return []
compile (ArrayIndexing Opt _) (JBool _) = return []
compile (ArrayIndexing Opt _) (JObject _) = return []
compile (ArrayIndexing _ (JNumberFilter (JNumber _))) JNull = return [JNull]
-- ArrayIndexing String -> turns out that's not a thing in Jq :(
-- compile (ArrayIndexing _ _) (JString []) = return [JNull]
-- compile (ArrayIndexing optio idx) (JString (x:xs))
--   | idx == 0     = return [JString [x]]
--   | idx > 0     = compile (ArrayIndexing optio (idx - 1)) (JString xs)
--   | otherwise    = compile (ArrayIndexing optio (-(idx + 1))) (JString (reverse xs))
-- ArrayIndexing Req with invalid filters
compile (ArrayIndexing Req (JNullFilter _)) _    = Left "Index must be number"
compile (ArrayIndexing Req (JStringFilter _)) _  = Left "Index must be number"
compile (ArrayIndexing Req (JBoolFilter _)) _    = Left "Index must be number"
compile (ArrayIndexing Req (JObjectFilter _)) _  = Left "Index must be number"
-- ArrayIndexing Req with invalid filters
compile (ArrayIndexing Opt (JNullFilter _)) _    = return []
compile (ArrayIndexing Opt (JStringFilter _)) _  = return []
compile (ArrayIndexing Opt (JBoolFilter _)) _    = return []
compile (ArrayIndexing Opt (JObjectFilter _)) _  = return []
-- For `echo '[10,20,30]' | jq '.[[10]]'` syntax where a filter array is used to index a JArray to get idx values
compile (ArrayIndexing _ (JArrayFilter [])) (JArray _) = return [JArray []]
compile (ArrayIndexing _ (JArrayFilter ((JNumberFilter jsonVal):_))) (JArray xs) = case elemIndex jsonVal xs of
  Nothing -> return [JArray []]
  Just n -> return [JArray [JNumber (fromIntegral n)]]
-- For `echo '[10,20,30]' | jq '.[["str"]]'`
compile (ArrayIndexing _ (JArrayFilter (_:_))) (JArray _) = return [JArray []]
-- compile (ArrayIndexing _ (JArrayFilter (fltr:_))) inp  = compile fltr inp
-- ArrayIndexing All
compile (ArrayIndexing _ _) (JArray []) = return [JNull]
compile (ArrayIndexing optio (JNumberFilter (JNumber idx))) (JArray (x : xs))
  | norm_idx == 0 = return [x]
  | otherwise = if idxValid
                then compile (ArrayIndexing optio (JNumberFilter (JNumber (norm_idx - 1)))) (JArray xs)
                else return [JNull]
  where
    (norm_idx, idxValid) = normalizeIndexDub idx (x : xs)
compile (ArrayIndexing optio filt) inp = do
  eval_idx <- compile filt inp
  let indexFilters = fmap getFilterFromJson eval_idx
  let something = [compile (ArrayIndexing optio json) inp | json <- indexFilters]
  fmap concat (sequence something)
-- ArraySlice Req
compile (ArraySlice Req _ _) (JNumber _) = Left "Cannot ArraySlice number"
compile (ArraySlice Req _ _) (JBool _) = Left "Cannot ArraySlice bool"
compile (ArraySlice Req _ _) (JObject _) = Left "Cannot ArraySlice object"
-- ArraySlice Opt
compile (ArraySlice Opt _ _) (JNumber _) = return []
compile (ArraySlice Opt _ _) (JBool _) = return []
compile (ArraySlice Opt _ _) (JObject _) = return []
-- ArraySlice range Req
compile (ArraySlice Req (Just (JBoolFilter _)) _) _    = Left "Slice bounds must be numbers"
compile (ArraySlice Req _ (Just (JBoolFilter _))) _    = Left "Slice bounds must be numbers"
compile (ArraySlice Req (Just (JObjectFilter _)) _) _  = Left "Slice bounds must be numbers"
compile (ArraySlice Req _ (Just (JObjectFilter _))) _  = Left "Slice bounds must be numbers"
compile (ArraySlice Req (Just (JArrayFilter _)) _) _  = Left "Slice bounds must be numbers"
compile (ArraySlice Req _ (Just (JArrayFilter _))) _  = Left "Slice bounds must be numbers"
-- ArraySlice range Opt
compile (ArraySlice Opt (Just (JBoolFilter _)) _) _    = return []
compile (ArraySlice Opt _ (Just (JBoolFilter _))) _    = return []
compile (ArraySlice Opt (Just (JObjectFilter _)) _) _  = return []
compile (ArraySlice Opt _ (Just (JObjectFilter _))) _  = return []
compile (ArraySlice Opt (Just (JArrayFilter _)) _) _  = return []
compile (ArraySlice Opt _ (Just (JArrayFilter _))) _  = return []
-- JNull special case
compile (ArraySlice _ (Just (JNumberFilter (JNumber _))) (Just (JNumberFilter (JNumber _)))) JNull = return [JNull]
---------------------------------------------------------------------------------------------------------------------------------------
-- ArraySlice JString
-- JSON Filter values
-- Desugar left open
compile (ArraySlice optio Nothing filt2) (JString xs) =
  compile (ArraySlice optio (Just (JNumberFilter (JNumber 0))) filt2) (JString xs)
compile (ArraySlice optio (Just (JNullFilter JNull)) filt2) (JString xs) =
  compile (ArraySlice optio (Just (JNumberFilter (JNumber 0))) filt2) (JString xs)
-- Desugar right open
compile (ArraySlice optio filt1 Nothing) (JString xs) =
  compile (ArraySlice optio filt1 (Just (JNumberFilter (JNumber (fromIntegral (length xs)))))) (JString xs)
compile (ArraySlice optio filt1 (Just (JNullFilter JNull))) (JString xs) =
  compile (ArraySlice optio filt1 (Just (JNumberFilter (JNumber (fromIntegral (length xs)))))) (JString xs)
-- Evaluate
compile (ArraySlice _ (Just (JNumberFilter (JNumber lo))) (Just (JNumberFilter (JNumber hi)))) (JString xs)
  | norm_hi <= norm_lo = return [JString []]
  | otherwise = return [JString (sliceList xs norm_lo norm_hi)]
  where
    (int_lo, int_hi) = (round lo, round hi)
    (norm_lo, norm_hi, _) = normalizeIndices (int_lo, int_hi) xs
-- Unevaluated Filters
compile (ArraySlice optio (Just lo_filt) (Just hi_filt)) (JString xs) = do
  eval_lo <- compile lo_filt (JString xs)
  eval_hi <- compile hi_filt (JString xs)
  let sliceTupleCombinations = (,) <$> eval_lo <*> eval_hi
  let sliceTupleFilters = fmap (\(j1, j2) -> (getFilterFromJson j1, getFilterFromJson j2)) sliceTupleCombinations
  let something = [ compile (ArraySlice optio (Just lo_json) (Just hi_json)) (JString xs) | (lo_json, hi_json) <- sliceTupleFilters]
  fmap concat (sequence something)
---------------------------------------------------------------------------------------------------------------------------------------
-- ArraySlice JArray
-- JSON Filter values
-- Desugar left open
compile (ArraySlice optio Nothing filt2) (JArray xs) =
  compile (ArraySlice optio (Just (JNumberFilter (JNumber 0))) filt2) (JArray xs)
compile (ArraySlice optio (Just (JNullFilter JNull)) filt2) (JArray xs) =
  compile (ArraySlice optio (Just (JNumberFilter (JNumber 0))) filt2) (JArray xs)
-- Desugar right open
compile (ArraySlice optio filt1 Nothing) (JArray xs) =
  compile (ArraySlice optio filt1 (Just (JNumberFilter (JNumber (fromIntegral (length xs)))))) (JArray xs)
compile (ArraySlice optio filt1 (Just (JNullFilter JNull))) (JArray xs) =
  compile (ArraySlice optio filt1 (Just (JNumberFilter (JNumber (fromIntegral (length xs)))))) (JArray xs)
-- Evaluate
compile (ArraySlice _ (Just (JNumberFilter (JNumber lo))) (Just (JNumberFilter (JNumber hi)))) (JArray xs)
  | norm_hi <= norm_lo = return [JArray []]
  | otherwise = return [JArray (sliceList xs norm_lo norm_hi)]
  where
    (int_lo, int_hi) = (round lo, round hi)
    (norm_lo, norm_hi, _) = normalizeIndices (int_lo, int_hi) xs
-- Unevaluated Filters
compile (ArraySlice optio (Just lo_filt) (Just hi_filt)) inp = do
  eval_lo <- compile lo_filt inp
  eval_hi <- compile hi_filt inp
  let sliceTupleCombinations = (,) <$> eval_lo <*> eval_hi
  let sliceTupleFilters = fmap (\(j1, j2) -> (getFilterFromJson j1, getFilterFromJson j2)) sliceTupleCombinations
  let something = [ compile (ArraySlice optio (Just lo_json) (Just hi_json)) inp | (lo_json, hi_json) <- sliceTupleFilters]
  fmap concat (sequence something)
---------------------------------------------------------------------------------------------------------------------------------------
-- Iterator Req
compile (Iterator Req idxs) JNull = return (replicate (length idxs) JNull)
compile (Iterator Req _) (JNumber _) = Left "Cannot Iterate number"
compile (Iterator Req _) (JString _) = Left "Cannot Iterate string"
compile (Iterator Req _) (JBool _) = Left "Cannot Iterate bool"
-- Iterator Opt
compile (Iterator Opt idxs) JNull = return (replicate (length idxs) JNull)
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
compile (Iterator _ _) (JObject _) = Left "Cannot index object with number"
-- compile (Iterator optio idxs) (JObject kvPairs) =
--   fmap concat (sequence indexedList)
--     where
--       values = map snd kvPairs
--       indexedList = map f idxs
--       f = \idx -> compile (ArrayIndexing optio (JNumberFilter (JNumber (fromIntegral idx)))) (JArray values)
-- Iterator Array
compile (Iterator optio idxs) (JArray values) =
  fmap concat (sequence indexedList)
    where
      indexedList = map f idxs
      f = \idx -> compile (ArrayIndexing optio (JNumberFilter (JNumber (fromIntegral idx)))) (JArray values)
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
compile RecDesc (JArray jsons) = do
  -- deeper <- sequence (fmap (compile RecDesc) jsons)
  deeper <- mapM (compile RecDesc) jsons
  return (JArray jsons : concat deeper)
-- RecDesc JObject
compile RecDesc (JObject kvpairs) = do
  -- deeper <- sequence (fmap (compile RecDesc) (map snd kvpairs))
  deeper <- mapM (compile RecDesc . snd) kvpairs
  return (JObject kvpairs : concat deeper)
-- RecDesc other JSONs
compile RecDesc inp = return [inp]
-- Value constructors
compile (JNullFilter jNull) _ = return [jNull]
compile (JNumberFilter jNumber) _ = return [jNumber]
compile (JBoolFilter jBool) _ = return [jBool]
compile (JStringFilter jString) _ = return [jString]
compile (JArrayFilter fltrs) inp = do
  jsons <- fmap concat (mapM (`compile` inp) fltrs)
  return [JArray jsons]
compile (JObjectFilter jObject) _ = return [jObject]
-- Conditionals and comparators
-- Equals
compile (Equals filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return (fmap JBool ((==) <$> res1 <*> res2))
-- NotEquals
compile (NotEquals filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return (fmap JBool ((/=) <$> res1 <*> res2))
-- LessThan
compile (LessThan filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return (fmap JBool ((<) <$> res1 <*> res2))
-- LessThanOrEqual
compile (LessThanOrEqual filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return (fmap JBool ((<=) <$> res1 <*> res2))
-- MoreThan
compile (MoreThan filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return (fmap JBool ((>) <$> res1 <*> res2))
-- MoreThanOrEqual
compile (MoreThanOrEqual filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return (fmap JBool ((>=) <$> res1 <*> res2))
-- And
compile (LogicalAnd filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return (fmap JBool (logicalMatchJson (&&) <$> res1 <*> res2))
-- Or
compile (LogicalOr filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return (fmap JBool (logicalMatchJson (||) <$> res1 <*> res2))
-- Not
compile LogicalNot json = Right [JBool (not (getJsonTruthValue json))]
-- IfThenElse
compile (IfThenElse cond caseTrue caseFalse) inp = do
  conds <- compile cond inp
  let trueOrFalses = map getJsonTruthValue conds
  let casesFilters = map (ifThenElseReturnCorrectCase caseTrue caseFalse) trueOrFalses
  fmap concat (mapM (`compile` inp) casesFilters)
-- Arithmetic
-- And
compile (Add filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return ((+) <$> res1 <*> res2)
-- Subt
compile (Subt filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return ((-) <$> res1 <*> res2)
-- Mult
compile (Mult filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return ((*) <$> res1 <*> res2)
-- Div
compile (Div filt1 filt2) inp = do
  res1 <- compile filt1 inp
  res2 <- compile filt2 inp
  return ((/) <$> res1 <*> res2)
-- Try Catch
compile (TryCatch filt1 filt2) inp = case compile filt1 inp of
  Left errStr -> compile filt2 (JString errStr)
  Right jss -> return jss


run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j

ifThenElseReturnCorrectCase :: p -> p -> Bool -> p
ifThenElseReturnCorrectCase caseTrue caseFalse cond = if cond then caseTrue else caseFalse

logicalMatchJson :: (Bool -> Bool -> Bool) -> JSON -> JSON -> Bool
logicalMatchJson op json1 json2 = case (getJsonTruthValue json1, getJsonTruthValue json2) of
  (b1, b2) -> op b1 b2

getJsonTruthValue :: JSON -> Bool
getJsonTruthValue JNull = False
getJsonTruthValue (JNumber _) = True
getJsonTruthValue (JString _) = True
getJsonTruthValue (JBool bool) = bool
getJsonTruthValue (JArray _) = True
getJsonTruthValue (JObject _) = True

getFilterFromJson :: JSON -> Filter
getFilterFromJson JNull = JNullFilter JNull
getFilterFromJson (JNumber n) = JNumberFilter (JNumber n)
getFilterFromJson (JString s) = JStringFilter (JString s)
getFilterFromJson (JBool bool) = JBoolFilter (JBool bool)
getFilterFromJson (JArray xs) = JArrayFilter (map getFilterFromJson xs)
-- TODO FINISH THE JOBJECT
getFilterFromJson (JObject kvp) = JObjectFilter (JObject kvp)

getJsonFromFilter :: Filter -> JSON
getJsonFromFilter (JNullFilter n) = n
getJsonFromFilter (JNumberFilter n) = n
getJsonFromFilter (JStringFilter s) = s
getJsonFromFilter (JBoolFilter bool) = bool
getJsonFromFilter (JArrayFilter xs) = JArray (map getJsonFromFilter xs)
-- TODO FINISH THE JOBJECT
getJsonFromFilter (JObjectFilter kvp) = kvp
