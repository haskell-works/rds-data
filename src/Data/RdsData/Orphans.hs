{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{- HLINT ignore "Use let" -}

module Data.RdsData.Orphans
  (
  ) where

import           Amazonka.Data                 (FromJSON, ToJSON)
import qualified Data.Aeson                    as J
import           Data.RdsData.Internal.Convert
import qualified Data.Text                     as T
import           Data.ULID                     (ULID)

instance FromJSON ULID where
  parseJSON = J.withText "ULID" $ \txt ->
    either (fail . T.unpack) return $ textToUlid txt

instance ToJSON ULID where
  toJSON = J.toJSON . show
