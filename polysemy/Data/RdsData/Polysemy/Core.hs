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
  => Member (Reader StatementContext) r
  => Text
  -> Sem r AWS.ExecuteStatement
newExecuteStatement sql = do
  context <- ask @StatementContext

  let AwsResourceArn theResourceArn = context ^. the @"resourceArn"
  let AwsSecretArn theSecretArn = context ^. the @"secretArn"

  pure $ AWS.newExecuteStatement theResourceArn theSecretArn sql
    & the @"database" .~ (context ^? the @"database" . _Just . the @1)

newBatchExecuteStatement :: ()
  => Member (Reader StatementContext) r
  => Text
  -> Sem r AWS.BatchExecuteStatement
newBatchExecuteStatement sql = do
  context <- ask @StatementContext

  let AwsResourceArn theResourceArn = context ^. the @"resourceArn"
  let AwsSecretArn theSecretArn = context ^. the @"secretArn"

  pure $ AWS.newBatchExecuteStatement theResourceArn theSecretArn sql
    & the @"database" .~ (context ^? the @"database" . _Just . the @1)

executeStatement :: ()
  => HasCallStack
  => Member (DataLog AwsLogEntry) r
  => Member (Embed m) r
  => Member (Error AWS.Error) r
  => Member (Error RdsDataError) r
  => Member (Reader StatementContext) r
  => Member (Reader Env) r
  => Member Log r
  => Member Resource r
  => MonadIO m
  => Text
  -> Sem r AWS.ExecuteStatementResponse
executeStatement sql = withFrozenCallStack do
  res <- newExecuteStatement sql >>= sendAws

  case res ^. the @"httpStatus" of
    200 -> do
      info $ "Successfully executed statement.  Results: " <> tshow res
      pure res
    _   -> throw $ RdsDataError $ "Failed to initialise database: " <> tshow res

executeStatement_ :: ()
  => HasCallStack
  => Member (DataLog AwsLogEntry) r
  => Member (Embed m) r
  => Member (Error AWS.Error) r
  => Member (Error RdsDataError) r
  => Member (Reader StatementContext) r
  => Member (Reader Env) r
  => Member Log r
  => Member Resource r
  => MonadIO m
  => Text
  -> Sem r ()
executeStatement_ f = withFrozenCallStack do
  void $ executeStatement f

initialiseDb :: ()
  => HasCallStack
  => Member (DataLog AwsLogEntry) r
  => Member (Embed m) r
  => Member (Error AWS.Error) r
  => Member (Error RdsDataError) r
  => Member (Reader StatementContext) r
  => Member (Reader Env) r
  => Member Log r
  => Member Resource r
  => MonadIO m
  => Sem r ()
initialiseDb = withFrozenCallStack do
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
    "CREATE INDEX IF NOT EXISTS idx_migration_created_at ON migration (created_at);"

  executeStatement_
    "CREATE INDEX IF NOT EXISTS idx_migration_deployed_by ON migration (deployed_by);"
