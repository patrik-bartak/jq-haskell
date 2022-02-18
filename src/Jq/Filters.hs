module Jq.Filters where

data Filter = Identity

instance Show Filter where
  show (Identity) = "."

instance Eq Filter where
  (==) = undefined

data Config = ConfigC {filters :: Filter}

-- Smart constructors
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterIndexingSC :: String -> Filter
filterIndexingSC = undefined

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = undefined

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = undefined
