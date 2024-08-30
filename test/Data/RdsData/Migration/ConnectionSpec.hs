{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant pure" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use let" -}

module Data.RdsData.Migration.ConnectionSpec
  ( tasty_rds_integration_test,
  ) where

import           Amazonka.Env                              (Env)
import qualified Amazonka.RDSData.ExecuteStatement         as AWS
import qualified Amazonka.Types                            as AWS
import           Control.Monad.IO.Class                    (MonadIO)
import           Data.Function
import           Data.Generics.Product.Any
import           Data.RdsData.Migration.Polysemy.Cluster
import           Data.RdsData.Migration.Polysemy.Env
import           Data.RdsData.Migration.Polysemy.Workspace
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Prelude
import           HaskellWorks.TestContainers.LocalStack
import           Lens.Micro
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log.Effect.DataLog               (DataLog)
import           Polysemy.Reader
import           Polysemy.Resource
import qualified Test.Tasty                                as Tasty
import qualified Test.Tasty.Hedgehog                       as H
import qualified TestContainers.Tasty                      as TC

newtype RdsDataMigrationError = RdsDataMigrationError Text
  deriving (Show)

executeStatement :: ()
  => Member (Error AWS.Error) r
  => Member (DataLog AwsLogEntry) r
  => Member (Embed m) r
  => Member (Error RdsDataMigrationError) r
  => Member (Reader Env) r
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Member Resource r
  => MonadIO m
  => Text
  -> Sem r AWS.ExecuteStatementResponse
executeStatement sql = do
  res <- newExecuteStatement sql >>= sendAws

  case res ^. the @"httpStatus" of
    200 -> pure res
    _   -> throw $ RdsDataMigrationError $ "Failed to initialise database: " <> tshow res

executeStatement_ :: ()
  => Member (Error AWS.Error) r
  => Member (DataLog AwsLogEntry) r
  => Member (Embed m) r
  => Member (Error RdsDataMigrationError) r
  => Member (Reader Env) r
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Member Resource r
  => MonadIO m
  => Text
  -> Sem r ()
executeStatement_ = void . executeStatement

initialiseDb :: ()
  => Member (DataLog AwsLogEntry) r
  => Member (Embed m) r
  => Member (Error AWS.Error) r
  => Member (Error RdsDataMigrationError) r
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Member (Reader Env) r
  => Member Resource r
  => MonadIO m
  => Sem r ()
initialiseDb = do
  executeStatement_ $ mconcat
    [ "CREATE TABLE IF NOT EXISTS migration ("
    , "  ulid CHAR(26)    NOT NULL PRIMARY KEY,"
    , "  created_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,"
    , "  deployed_by      TEXT NOT NULL,"
    , "  CONSTRAINT valid_ulid_constraint"
    , "    CHECK (ulid::text ~ '^[0-9A-HJKMNP-TV-Z]{26}$')"
    , ");"
    ]

  executeStatement_
    "CREATE INDEX idx_migration_created_at ON migration (created_at);"

  executeStatement_
    "CREATE INDEX idx_migration_deployed_by ON migration (deployed_by);"

-- cabal test rds-data-migration-test --test-options "--pattern \"/RDS integration test/\""
tasty_rds_integration_test :: Tasty.TestTree
tasty_rds_integration_test =
  TC.withContainers setupContainers $ \getContainer ->
    H.testProperty "RDS integration test" $ propertyOnce $ localWorkspace $ runLocalTestEnv getContainer $ do
      rdsClusterDetails <- createRdsDbCluster getContainer

      runReaderResourceAndSecretArnsFromResponses rdsClusterDetails $ do
        id $ do
          initialiseDb
            & trapFail @RdsDataMigrationError
            & trapFail @AWS.Error
            & jotShowDataLog @AwsLogEntry

        id $ do
          res <- newExecuteStatement "INSERT INTO migration (ulid, deployed_by) VALUES ('01J6H2CDMJERDTFYP7QQPBZE0C', 'John Doe')"
            >>= sendAws
            & jotShowDataLog @AwsLogEntry
            & trapFail
            & jotShowM

          jotShow_ $ res ^. the @"httpStatus"

        id $ do
          res <- newExecuteStatement "SELECT ulid FROM migration"
            >>= sendAws
            & jotShowDataLog @AwsLogEntry
            & trapFail
            & jotShowM

          jotShow_ $ res ^. the @"httpStatus"

          numRecords <- jotShow $ res ^? the @"records" . _Just . to length

          numRecords === Just 1

        id $ do
          res <- newExecuteStatement "CREATE TABLE Users (name VARCHAR(50))"
            >>= sendAws
            & jotShowDataLog @AwsLogEntry
            & trapFail
            & jotShowM

          jotShow_ $ res ^. the @"httpStatus"

        id $ do
          res <- newExecuteStatement "INSERT INTO Users (name) VALUES ('John Doe')"
            >>= sendAws
            & jotShowDataLog @AwsLogEntry
            & trapFail
            & jotShowM

          jotShow_ $ res ^. the @"httpStatus"

        id $ do
          res <- newExecuteStatement "SELECT name FROM Users"
            >>= sendAws
            & jotShowDataLog @AwsLogEntry
            & trapFail
            & jotShowM

          jotShow_ $ res ^. the @"httpStatus"

          numRecords <- jotShow $ res ^? the @"records" . _Just . to length

          numRecords === Just 1
