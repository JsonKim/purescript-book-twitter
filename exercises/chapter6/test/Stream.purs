module Test.Stream where

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.String.CodeUnits as String

class Stream stream element | stream -> element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }

instance streamArray :: Stream (Array a) a where
  uncons = Array.uncons

instance streamString :: Stream String Char where
  uncons = String.uncons
