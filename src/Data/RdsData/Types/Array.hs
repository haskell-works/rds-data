{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use <&>" -}

module Data.RdsData.Types.Array
  ( Array(..)
  , fromArrayValue
  , toArrayValue
  ) where

import           Control.Applicative
import           Data.Generics.Product.Any
import           Data.Text
import           GHC.Generics
import           Lens.Micro

import qualified Amazonka.RDSData          as AWS
import qualified Data.Aeson                as J

data Array =
    ArrayOfArrays   [Array]
  | ArrayOfBools    [Bool]
  | ArrayOfDoubles  [Double]
  | ArrayOfIntegers [Integer]
  | ArrayOfTexts    [Text]
  deriving (Eq, Generic, Show)

instance J.ToJSON Array where

fromArrayValue :: AWS.ArrayValue -> Maybe Array
fromArrayValue v =
  asum
    [ v ^. the @"booleanValues" >>= pure . ArrayOfBools
    , v ^. the @"doubleValues"  >>= pure . ArrayOfDoubles
    , v ^. the @"longValues"    >>= pure . ArrayOfIntegers
    , v ^. the @"stringValues"  >>= pure . ArrayOfTexts
    , v ^. the @"arrayValues"   >>= fmap ArrayOfArrays . traverse fromArrayValue
    ]

toArrayValue :: Array -> AWS.ArrayValue
toArrayValue = \case
  ArrayOfArrays   vs -> AWS.newArrayValue & the @"arrayValues"    .~ Just (toArrayValue <$> vs)
  ArrayOfBools    vs -> AWS.newArrayValue & the @"booleanValues"  .~ Just vs
  ArrayOfDoubles  vs -> AWS.newArrayValue & the @"doubleValues"   .~ Just vs
  ArrayOfIntegers vs -> AWS.newArrayValue & the @"longValues"     .~ Just vs
  ArrayOfTexts    vs -> AWS.newArrayValue & the @"stringValues"   .~ Just vs
