{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use <&>" -}

module Data.RdsData.Types.Value
  ( Value(..)
  , fromField
  , toField
  ) where

import           Amazonka.Data.Base64
import           Control.Applicative
import           Data.Generics.Product.Any
import           Data.RdsData.Internal.Maybe
import           Data.RdsData.Types.Array
import           Data.Text
import           GHC.Generics
import           Lens.Micro

import qualified Amazonka.RDSData            as AWS
import qualified Data.Aeson                  as J

data Value =
    ValueOfArray    Array
  | ValueOfBase64   Base64
  | ValueOfBool     Bool
  | ValueOfDouble   Double
  | ValueOfInteger  Integer
  | ValueOfText     Text
  | ValueOfNull
  deriving (Eq, Generic, Show)

instance J.ToJSON Value where

fromField :: AWS.Field -> Maybe Value
fromField field =
  asum
    [ field ^. the @"arrayValue"    >>= fromArrayValue >>= pure . ValueOfArray
    , field ^. the @"blobValue"     >>= pure . ValueOfBase64
    , field ^. the @"booleanValue"  >>= pure . ValueOfBool
    , field ^. the @"doubleValue"   >>= pure . ValueOfDouble
    , field ^. the @"longValue"     >>= pure . ValueOfInteger
    , field ^. the @"stringValue"   >>= pure . ValueOfText
    , field ^. the @"isNull"        >>= toMaybe ValueOfNull
    ]

toField :: Value -> AWS.Field
toField = \case
  ValueOfArray    v -> AWS.newField & the @"arrayValue" .~ Just (toArrayValue v)
  ValueOfBase64   v -> AWS.newField & the @"blobValue" .~ Just v
  ValueOfBool     v -> AWS.newField & the @"booleanValue" .~ Just v
  ValueOfDouble   v -> AWS.newField & the @"doubleValue" .~ Just v
  ValueOfInteger  v -> AWS.newField & the @"longValue" .~ Just v
  ValueOfText     v -> AWS.newField & the @"stringValue" .~ Just v
  ValueOfNull       -> AWS.newField & the @"isNull" .~ Just True
