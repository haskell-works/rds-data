{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use <&>" -}

module Data.RdsData.Types.Param
  ( Param(..)
  , toSqlParameter
  ) where

import           Data.Generics.Product.Any
import           Data.RdsData.Types.Value
import           Data.Text
import           GHC.Generics
import           Lens.Micro

import qualified Amazonka.RDSData          as AWS
import qualified Data.Aeson                as J

data Param = Param
  { name  :: Maybe Text
  , hint  :: Maybe AWS.TypeHint
  , value :: Value
  } deriving (Eq, Generic, Show)

instance J.ToJSON Param where

toSqlParameter :: Param -> AWS.SqlParameter
toSqlParameter (Param n h v) =
  AWS.newSqlParameter & the @"name" .~ n & the @"typeHint" .~ h & the @"value" ?~ toField v
