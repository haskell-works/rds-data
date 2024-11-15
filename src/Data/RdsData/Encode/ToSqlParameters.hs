module Data.RdsData.Encode.ToSqlParameters
  ( ToSqlParameters(..)
  ) where

import           Data.RdsData.Encode.Params
import           Data.RdsData.Types.Param

import qualified Amazonka.RDSData           as AWS

class ToSqlParameters a where
  toSqlParameters :: a -> [AWS.SqlParameter]

instance ToSqlParameters [AWS.SqlParameter] where
  toSqlParameters = id

instance ToSqlParameters EncodedParams where
  toSqlParameters encodedParams =
    fmap toSqlParameter (encodedParams.run [])
