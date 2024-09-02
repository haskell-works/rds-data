{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- HLINT ignore "Use let" -}

module Data.RdsData.Migration.Types
  ( MigrationRow(..),
    RdsClusterDetails(..),
    Migration(..),
    Step(..),
    Statement(..),
  ) where

import           Amazonka.Data           (FromJSON, ToJSON)
import qualified Amazonka.RDS            as AWS
import qualified Amazonka.SecretsManager as AWS
import           Data.RdsData.Orphans    ()
import           Data.Text               (Text)
import           Data.Time
import           Data.ULID               (ULID)
import           GHC.Generics

data MigrationRow = MigrationRow
  { uuid       :: ULID
  , createdBy  :: UTCTime
  , deployedBy :: Text
  } deriving (Eq, Show, Generic)

data RdsClusterDetails = RdsClusterDetails
  { createDbClusterResponse :: AWS.CreateDBClusterResponse
  , createSecretResponse    :: AWS.CreateSecretResponse
  } deriving (Eq, Generic, Show)

data Migration = Migration
  { description :: Text
  , plan        :: [Step]
  }
  deriving (Eq, Generic, Show)

instance ToJSON Migration

instance FromJSON Migration

data Step = Step
  { id          :: ULID
  , description :: Text
  , up          :: [Statement]
  , down        :: [Statement]
  }
  deriving (Eq, Generic, Show)

instance ToJSON Step

instance FromJSON Step

newtype Statement = Statement Text
  deriving (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON)
