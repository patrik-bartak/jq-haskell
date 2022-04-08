module Jq.Parser (module Jq.JParser, module Jq.CParser, parse) where

import Jq.CParser
import Jq.JParser
import qualified Parsing.Parsing as P (Parser, parse)

parse :: P.Parser a -> String -> Maybe a
parse p s = case (P.parse p s) of
  [(v, "")] -> Just v
  _ -> Nothing
