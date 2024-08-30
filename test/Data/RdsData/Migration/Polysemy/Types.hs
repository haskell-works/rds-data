{-# LANGUAGE DeriveGeneric #-}

module Data.RdsData.Migration.Polysemy.Types
  ( RdsClusterDetails(..),
  ) where

import qualified Amazonka.RDS            as AWS
import qualified Amazonka.SecretsManager as AWS
import           GHC.Generics

data RdsClusterDetails = RdsClusterDetails
  { createDbClusterResponse :: AWS.CreateDBClusterResponse
  , createSecretResponse    :: AWS.CreateSecretResponse
  } deriving (Eq, Generic, Show)
