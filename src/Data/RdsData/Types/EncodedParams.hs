module Data.RdsData.Types.EncodedParams
  ( EncodedParams(..)
  ) where

import           Data.RdsData.Types.Param

newtype EncodedParams = EncodedParams
  { run :: [Param] -> [Param]
  }

instance Semigroup EncodedParams where
  EncodedParams f <> EncodedParams g =
    EncodedParams (f . g)

instance Monoid EncodedParams where
  mempty =
    EncodedParams id
