{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- HLINT ignore "Use let" -}

module Data.RdsData.Migration.Types
  ( MigrationRow(..)
  ) where

import Data.Text (Text)
import Data.Time
import Data.ULID (ULID)
import GHC.Generics

data MigrationRow = MigrationRow
  { uuid        :: ULID
  , createdBy   :: UTCTime
  , deployedBy  :: Text
  } deriving (Eq, Show, Generic)
