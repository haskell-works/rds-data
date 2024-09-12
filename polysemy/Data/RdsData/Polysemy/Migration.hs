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
import qualified Data.Aeson                     as J
import qualified Data.ByteString.Lazy           as LBS
import           Data.Generics.Product.Any
import           Data.RdsData.Aws
import           Data.RdsData.Migration.Types   hiding (id)
import           Data.RdsData.Polysemy.Core
import           Data.RdsData.Polysemy.Error
import qualified Data.Text.Encoding             as T
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
  => Member (Reader StatementContext) r
  => Member Log r
  => Member Resource r
  => FilePath
  -> Sem r ()
migrateDown migrationFp = do
  value :: Migration <- readYamlFile migrationFp

  let statements = value ^.. the @"plan" . to reverse . each . the @"down" . each

  forM_ statements $ \statement -> do
    info $ "Executing statement: " <> tshow statement

    response <- executeStatement (statement ^. the @1)

    info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))

migrateUp :: ()
  => Member (DataLog AwsLogEntry) r
  => Member (Embed IO) r
  => Member (Error AWS.Error) r
  => Member (Error IOException) r
  => Member (Error JsonDecodeError) r
  => Member (Error RdsDataError) r
  => Member (Error YamlDecodeError) r
  => Member (Reader AWS.Env) r
  => Member (Reader StatementContext) r
  => Member Log r
  => Member Resource r
  => FilePath
  -> Sem r ()
migrateUp migrationFp = do
  value :: Migration <- readYamlFile migrationFp

  let statements = value ^.. the @"plan" . each . the @"up" . each

  forM_ statements $ \statement -> do
    info $ "Executing statement: " <> tshow statement

    response <- executeStatement (statement ^. the @1)

    info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))
