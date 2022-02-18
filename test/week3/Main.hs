{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
module Main where

import           Control.Monad
import           Control.DeepSeq
import           Data.List
import           GHC.Generics (Generic, Generic1)
import           System.Exit
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

import           Jq.Json                              as Json

{--
  It can be that one (or both) of these two derivation fail.
  Especially if you introduce some non-trivial constructors
  or if your definition of filter is mutually recursive with
  some other definition.
  This doesn't necessarily mean that you're doing anything wrong.
  You can try fixing it yourself by adding
  `deriving instance Generic X` and
  `deriving instance NFData X` below for the missing classes.
  In case this doesn't work reach out to the course team.
--}
deriving instance Generic JSON
deriving instance NFData JSON

instance Arbitrary JSON where
    arbitrary = do
        n <- arbitrary :: Gen Int
        s <- arbitrary :: Gen String
        b <- arbitrary :: Gen Bool
        xs <- frequency [ (1, return []), (5, do { x <- arbitrary; return [x] })]
        ys <- frequency [ (1, return []), (5, do { x <- arbitrary; s <- arbitrary; return [(s, x)] })]

        elements [ jsonNullSC, jsonNumberSC n, jsonStringSC s, jsonBoolSC b, jsonArraySC xs, jsonObjectSC ys ]

main = defaultMain tests


prop_computes_null  = total $ jsonNullSC
prop_computes_number n = total $ jsonNumberSC n
prop_computes_string s = total $ jsonStringSC s
prop_computes_bool b = total $ jsonBoolSC b
prop_computes_array xs = total $ jsonArraySC xs
prop_computes_object xs = total $ jsonObjectSC xs

prop_null_refl      = jsonNullSC      == jsonNullSC
prop_number_refl n  = jsonNumberSC n  == jsonNumberSC n
prop_string_refl s  = jsonStringSC s  == jsonStringSC s
prop_bool_refl   b  = jsonBoolSC b    == jsonBoolSC b
prop_array_refl  xs = jsonArraySC xs  == jsonArraySC xs
prop_object_refl xs = jsonObjectSC xs == jsonObjectSC xs

prop_null_not_equals_number n  = jsonNullSC /= jsonNumberSC n
prop_null_not_equals_string s  = jsonNullSC /= jsonStringSC s
prop_null_not_equals_bool   b  = jsonNullSC /= jsonBoolSC b
prop_null_not_equals_array  xs = jsonNullSC /= jsonArraySC xs
prop_null_not_equals_object xs = jsonNullSC /= jsonObjectSC xs

prop_number_not_equals_null   n    = jsonNumberSC n /= jsonNullSC
prop_number_not_equals_string n s  = jsonNumberSC n /= jsonStringSC s
prop_number_not_equals_bool   n b  = jsonNumberSC n /= jsonBoolSC b
prop_number_not_equals_array  n xs = jsonNumberSC n /= jsonArraySC xs
prop_number_not_equals_object n xs = jsonNumberSC n /= jsonObjectSC xs

prop_string_not_equals_null   s    = jsonStringSC s /= jsonNullSC
prop_string_not_equals_number s n  = jsonStringSC s /= jsonNumberSC n
prop_string_not_equals_bool   s b  = jsonStringSC s /= jsonBoolSC b
prop_string_not_equals_array  s xs = jsonStringSC s /= jsonArraySC xs
prop_string_not_equals_object s xs = jsonStringSC s /= jsonObjectSC xs

prop_bool_not_equals_null   b    = jsonBoolSC b /= jsonNullSC
prop_bool_not_equals_number b n  = jsonBoolSC b /= jsonNumberSC n
prop_bool_not_equals_string b s  = jsonBoolSC b /= jsonStringSC s
prop_bool_not_equals_array  b xs = jsonBoolSC b /= jsonArraySC xs
prop_bool_not_equals_object b xs = jsonBoolSC b /= jsonObjectSC xs

prop_array_not_equals_null   xs    = jsonArraySC xs /= jsonNullSC
prop_array_not_equals_number xs n  = jsonArraySC xs /= jsonNumberSC n
prop_array_not_equals_string xs s  = jsonArraySC xs /= jsonStringSC s
prop_array_not_equals_bool   xs b  = jsonArraySC xs /= jsonBoolSC b
prop_array_not_equals_object xs ys = jsonArraySC xs /= jsonObjectSC ys

prop_object_not_equals_null   xs    = jsonObjectSC xs /= jsonNullSC
prop_object_not_equals_number xs n  = jsonObjectSC xs /= jsonNumberSC n
prop_object_not_equals_string xs s  = jsonObjectSC xs /= jsonStringSC s
prop_object_not_equals_bool   xs b  = jsonObjectSC xs /= jsonBoolSC b
prop_object_not_equals_array  xs ys = jsonObjectSC xs /= jsonArraySC ys

prop_show_null               = show jsonNullSC == "null"
prop_show_number n           = show (jsonNumberSC n) == show n
prop_show_string s           = show (jsonStringSC s) == '\"' : s ++ "\""
prop_show_bool_true          = show (jsonBoolSC True) == "true"
prop_show_bool_false         = show (jsonBoolSC False) == "false"
prop_show_empty_array        = show (jsonArraySC []) == "[]"
prop_show_array_one_element  = show (jsonArraySC [jsonNullSC]) == "[\n  null\n]"
prop_show_empty_object       = show (jsonObjectSC []) == "{}"
prop_show_object_one_element = show (jsonObjectSC [("key", jsonNullSC)]) == "{\n  \"key\": null\n}"

tests = [
    testGroup "Constructors are defined" [
        testProperty "Constructor for null computes" prop_computes_null
      , testProperty "Constructor for numbers computes" prop_computes_number
      , testProperty "Constructor for strings computes" prop_computes_string
      , testProperty "Constructor for booleans computes" prop_computes_bool
      , testProperty "Constructor for array computes" prop_computes_array
      , testProperty "Constructor for objects computes" prop_computes_object
    ],
    testGroup "Reflection instances" [
        testProperty "Reflection null" prop_null_refl
      , testProperty "Reflection number" prop_number_refl
      , testProperty "Reflection string" prop_string_refl
      , testProperty "Reflection bool" prop_bool_refl
      , testProperty "Reflection array" prop_array_refl
      , testProperty "Reflection object" prop_object_refl
    ],
    testGroup "Inequality null" [
        testProperty "Inequality null number" prop_null_not_equals_number
      , testProperty "Inequality null string" prop_null_not_equals_string
      , testProperty "Inequality null bool" prop_null_not_equals_bool
      , testProperty "Inequality null array" prop_null_not_equals_array
      , testProperty "Inequality null object" prop_null_not_equals_object
    ],
    testGroup "Inequality number" [
        testProperty "Inequality number null" prop_number_not_equals_null
      , testProperty "Inequality number string" prop_number_not_equals_string
      , testProperty "Inequality number bool" prop_number_not_equals_bool
      , testProperty "Inequality number array" prop_number_not_equals_array
      , testProperty "Inequality number object" prop_number_not_equals_object
    ],
    testGroup "Inequality string" [
        testProperty "Inequality string null" prop_string_not_equals_null
      , testProperty "Inequality string number" prop_string_not_equals_number
      , testProperty "Inequality string bool" prop_string_not_equals_bool
      , testProperty "Inequality string array" prop_string_not_equals_array
      , testProperty "Inequality string object" prop_string_not_equals_object
    ],
    testGroup "Inequality bool" [
        testProperty "Inequality bool null" prop_bool_not_equals_null
      , testProperty "Inequality bool number" prop_bool_not_equals_number
      , testProperty "Inequality bool string" prop_bool_not_equals_string
      , testProperty "Inequality bool array" prop_bool_not_equals_array
      , testProperty "Inequality bool object" prop_bool_not_equals_object
    ],
    testGroup "Inequality array" [
        testProperty "Inequality array null" prop_array_not_equals_null
      , testProperty "Inequality array number" prop_array_not_equals_number
      , testProperty "Inequality array string" prop_array_not_equals_string
      , testProperty "Inequality array bool" prop_array_not_equals_bool
      , testProperty "Inequality array object" prop_array_not_equals_object
    ],
    testGroup "Inequality object" [
        testProperty "Inequality object null" prop_object_not_equals_null
      , testProperty "Inequality object number" prop_object_not_equals_number
      , testProperty "Inequality object string" prop_object_not_equals_string
      , testProperty "Inequality object bool" prop_object_not_equals_bool
      , testProperty "Inequality object array" prop_object_not_equals_array
    ],
    testGroup "Show instances" [
        testProperty "Show null" prop_show_null
      , testProperty "Show number" prop_show_number
      , testProperty "Show string" prop_show_string
      , testProperty "Show true" prop_show_bool_true
      , testProperty "Show false" prop_show_bool_false
      , testProperty "Show empty array" prop_show_empty_array
      , testProperty "Show array one element" prop_show_array_one_element
      , testProperty "Show empty object" prop_show_empty_object
      , testProperty "Show object one element" prop_show_object_one_element
    ]]
