{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

{- HLINT ignore "Use let" -}

module App.Cli.Run.Describe
  ( runDescribeCmd
  ) where

import           Amazonka.RDSData
import           App.AWS.Env
import           App.Config
import           Control.Monad.IO.Class
import           Data.Generics.Product.Any
import           Data.Maybe
import           Data.RdsData.Migration.Types (MigrationRow (..))
import           Data.RdsData.Types
import           Lens.Micro

import qualified Amazonka                     as AWS
import qualified App.Cli.Types                as CLI
import qualified App.Console                  as T
import qualified Data.Aeson                   as J
import qualified Data.RdsData.Decode.Row      as DEC
import qualified Data.Text                    as T
import qualified Data.Text.Lazy.Encoding      as LT
import qualified Data.Text.Lazy.IO            as LT
import qualified System.IO                    as IO
import qualified System.IO.Unsafe             as IO

runDescribeCmd :: CLI.DescribeCmd -> IO ()
runDescribeCmd cmd = do
  let theAwsLogLevel   = cmd ^. the @"mAwsLogLevel"
  let theMHostEndpoint = cmd ^. the @"mHostEndpoint"
  let theRegion        = cmd ^. the @"region"
  let theResourceArn   = cmd ^. the @"resourceArn"
  let theSecretArn     = cmd ^. the @"secretArn"

  envAws <-
    liftIO (IO.unsafeInterleaveIO (mkEnv theRegion (awsLogger theAwsLogLevel)))
      <&> applyMHostEndpoint theMHostEndpoint

  AWS.runResourceT $ do
    sql <- pure $ T.pack $ unlines
      [ "CREATE TABLE IF NOT EXISTS migration ("
      , "  uuid CHAR(26)    NOT NULL PRIMARY KEY,"
      , "  created_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,"
      , "  deployed_by      TEXT NOT NULL,"
      , "  CONSTRAINT valid_ulid_constraint"
      , "    CHECK (uuid::text ~ '^[0-9A-HJKMNP-TV-Z]{26}$'),"
      , "  INDEX idx_migration_created_at (created_at),"
      , "  INDEX idx_migration_deployed_by (deployed_by)"
      , ");"
      ]

    req <- pure $ newExecuteStatement theResourceArn theSecretArn sql

    res <- AWS.send envAws req

    liftIO . LT.putStrLn $ LT.decodeUtf8 $ J.encode res

  AWS.runResourceT $ do
    sql <- pure $ T.pack $ unlines
      [ "SELECT"
      , "  uuid,"
      , "  created_at,"
      , "  deployed_by"
      , "FROM migration"
      ]

    liftIO $ T.putStrLn sql

    req <- pure $ newExecuteStatement theResourceArn theSecretArn sql

    res <- AWS.send envAws req

    liftIO . LT.putStrLn $ LT.decodeUtf8 $ J.encode res

    decodeMigrationRow <- pure $ id @(DEC.DecodeRow MigrationRow) $
      MigrationRow
        <$> DEC.ulid
        <*> DEC.utcTime
        <*> DEC.text

    records <- pure $ id @[[Value]] $ fromMaybe [] $ mapM (mapM fromField) =<< res ^. the @"records"

    row <- pure $ DEC.decodeRows decodeMigrationRow records

    liftIO $ IO.print row

  pure ()
