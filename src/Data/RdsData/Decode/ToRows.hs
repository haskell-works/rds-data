module Data.RdsData.Decode.ToRows
  ( ToRows(..)
  ) where

import qualified Amazonka.RDSData          as AWS
import           Data.Generics.Product.Any
import           Data.Maybe
import           Data.RdsData.Types
import           Lens.Micro                ((^.))

class ToRows a where
    toRows :: a -> [[Value]]

instance ToRows [[Value]] where
    toRows = id

instance ToRows AWS.ExecuteStatementResponse where
    toRows res =
        fromMaybe [] $ mapM (mapM fromField) =<< res ^. the @"records"
