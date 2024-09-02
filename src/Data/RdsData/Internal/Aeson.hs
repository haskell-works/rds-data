{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.RdsData.Internal.Aeson where

import Data.Aeson
import Data.Text (Text)

import qualified Amazonka.RDSData     as AWS
import qualified Data.Aeson           as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as T

infixr 8 .=!
infixr 8 .=?

-- | A key-value pair difference list for encoding a JSON object.
(.=!) :: (KeyValue e kv, ToJSON v) => Key -> v -> [kv] -> [kv]
(.=!) n v = (n .= v:)

-- | A key-value pair difference list for encoding a JSON object where Nothing encodes absence of the key-value pair.
(.=?) :: (KeyValue e kv, ToJSON v) => Key -> Maybe v -> [kv] -> [kv]
(.=?) n mv = case mv of
  Just v -> (n .= v:)
  Nothing -> id

toJsonText :: ToJSON a => a -> Text
toJsonText field = T.decodeUtf8 (LBS.toStrict (J.encode field))

instance ToJSON AWS.ColumnMetadata where

instance ToJSON AWS.UpdateResult where

-- Customized options for toJSON
myOptions :: Options
myOptions = defaultOptions { omitNothingFields = True }

instance ToJSON AWS.ExecuteStatementResponse where
  toJSON = genericToJSON myOptions

instance FromJSON AWS.ExecuteStatementResponse where

instance ToJSON AWS.BatchExecuteStatementResponse where
  toJSON = genericToJSON myOptions

instance FromJSON AWS.BatchExecuteStatementResponse where
