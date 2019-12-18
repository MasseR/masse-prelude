{-|
Module      : MyPrelude.String
Description : String related prelude
Copyright   : (c) Mats Rauhala, 2019
License     : MIT
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

'String' related functions that could be a part of a prelude. Prefer using 'Data.Text' versions instead.
-}
module MyPrelude.String
  (
    toLower
  , toUpper
  )
  where

import qualified Data.Char as C

-- | Convert a string to lowercase
toLower :: String -> String
toLower = fmap C.toLower


-- | Convert a string to uppercase
toUpper :: String -> String
toUpper = fmap C.toUpper
