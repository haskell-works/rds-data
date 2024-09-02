{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.RdsData.Polysemy.Core
  ( executeStatement,
    executeStatement_,
    initialiseDb,
    newExecuteStatement,
    newBatchExecuteStatement,
  ) where

import           Amazonka.Env                           (Env)
import qualified Amazonka.RDSData.BatchExecuteStatement as AWS
import qualified Amazonka.RDSData.ExecuteStatement      as AWS
import qualified Amazonka.Types                         as AWS
import           Control.Monad.IO.Class                 (MonadIO)
import           Data.Generics.Product.Any
import           Data.RdsData.Aws
import           Data.RdsData.Polysemy.Error
import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Prelude
import           Lens.Micro

newExecuteStatement :: ()
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Text
  -> Sem r AWS.ExecuteStatement
newExecuteStatement sql = do
  AwsResourceArn theResourceArn <- ask
  AwsSecretArn theSecretArn <- ask

  pure $ AWS.newExecuteStatement theResourceArn theSecretArn sql

newBatchExecuteStatement :: ()
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Text
  -> Sem r AWS.BatchExecuteStatement
newBatchExecuteStatement sql = do
  AwsResourceArn theResourceArn <- ask
  AwsSecretArn theSecretArn <- ask

  pure $ AWS.newBatchExecuteStatement theResourceArn theSecretArn sql

executeStatement :: ()
  => Member (DataLog AwsLogEntry) r
  => Member (Embed m) r
  => Member (Error AWS.Error) r
  => Member (Error RdsDataError) r
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Member (Reader Env) r
  => Member Log r
  => Member Resource r
  => MonadIO m
  => Text
  -> Sem r AWS.ExecuteStatementResponse
executeStatement sql = do
  res <- newExecuteStatement sql >>= sendAws

  case res ^. the @"httpStatus" of
    200 -> do
      info $ "Successfully executed statement.  Results: " <> tshow res
      pure res
    _   -> throw $ RdsDataError $ "Failed to initialise database: " <> tshow res

executeStatement_ :: ()
  => Member (DataLog AwsLogEntry) r
  => Member (Embed m) r
  => Member (Error AWS.Error) r
  => Member (Error RdsDataError) r
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Member (Reader Env) r
  => Member Log r
  => Member Resource r
  => MonadIO m
  => Text
  -> Sem r ()
executeStatement_ = void . executeStatement

initialiseDb :: ()
  => Member (DataLog AwsLogEntry) r
  => Member (Embed m) r
  => Member (Error AWS.Error) r
  => Member (Error RdsDataError) r
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Member (Reader Env) r
  => Member Log r
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
