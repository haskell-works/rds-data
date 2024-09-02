{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.RdsData.Polysemy.Migration
  ( migrateDown,
    migrateUp,
  ) where

import qualified Amazonka.Env                   as AWS
import qualified Amazonka.Types                 as AWS
import           Data.Generics.Product.Any
import           Data.RdsData.Aws
import           Data.RdsData.Migration.Types   hiding (id)
import           Data.RdsData.Polysemy.Core
import           Data.RdsData.Polysemy.Error
import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.File
import           HaskellWorks.Polysemy.Prelude
import           Lens.Micro

migrateDown :: ()
  => Member (DataLog AwsLogEntry) r
  => Member (Embed IO) r
  => Member (Error AWS.Error) r
  => Member (Error IOException) r
  => Member (Error JsonDecodeError) r
  => Member (Error RdsDataError) r
  => Member (Error YamlDecodeError) r
  => Member (Reader AWS.Env) r
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Member Log r
  => Member Resource r
  => FilePath
  -> Sem r ()
migrateDown _migrationFp = do
  value :: Migration <- readYamlFile "db/migration.yaml"

  let statements = value ^.. the @"plan" . to reverse . each . the @"down" . each

  forM_ statements $ \statement -> do
    info $ "Executing statement: " <> tshow statement

    executeStatement (statement ^. the @1)

migrateUp :: ()
  => Member (DataLog AwsLogEntry) r
  => Member (Embed IO) r
  => Member (Error AWS.Error) r
  => Member (Error IOException) r
  => Member (Error JsonDecodeError) r
  => Member (Error RdsDataError) r
  => Member (Error YamlDecodeError) r
  => Member (Reader AWS.Env) r
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Member Log r
  => Member Resource r
  => FilePath
  -> Sem r ()
migrateUp _migrationFp = do
  value :: Migration <- readYamlFile "db/migration.yaml"

  let statements = value ^.. the @"plan" . each . the @"up" . each

  forM_ statements $ \statement -> do
    info $ "Executing statement: " <> tshow statement

    executeStatement (statement ^. the @1)
