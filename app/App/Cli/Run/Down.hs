{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}

module App.Cli.Run.Down
  ( runDownCmd
  ) where

import           Control.Monad.IO.Class
import           Data.Generics.Product.Any
import           Data.Maybe
import           Data.RdsData.Migration.Types    (MigrationRow (..))
import           Data.RdsData.Types
import           Lens.Micro

import qualified Amazonka                        as AWS
import qualified App.Cli.Types                   as CLI
import qualified App.Console                     as T
import qualified Data.Aeson                      as J
import           Data.RdsData.Aws
import qualified Data.RdsData.Decode.Row         as DEC
import           Data.RdsData.Polysemy.Core
import           Data.RdsData.Polysemy.Error
import           Data.RdsData.Polysemy.Migration
import qualified Data.Text                       as T
import qualified Data.Text.Lazy.Encoding         as LT
import qualified Data.Text.Lazy.IO               as LT
import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.File
import           HaskellWorks.Polysemy.Log
import           HaskellWorks.Prelude
import           Polysemy.Log
import           Polysemy.Time.Interpreter.Ghc
import qualified System.IO                       as IO

newtype AppError
  = AppError Text
  deriving (Eq, Show)

runApp :: ()
  => CLI.DownCmd
  -> Sem
      [ Reader AwsResourceArn
      , Reader AwsSecretArn
      , Reader AWS.Env
      , Error AppError
      , DataLog AwsLogEntry
      , Log
      , GhcTime
      , DataLog (LogEntry LogMessage)
      , Resource
      , Embed IO
      , Final IO
      ] ()
  -> IO ()
runApp cmd f = f
  & runReader (AwsResourceArn $ cmd ^. the @"resourceArn")
  & runReader (AwsSecretArn $ cmd ^. the @"secretArn")
  & runReaderAwsEnvDiscover
  & trap @AppError reportFatal
  & interpretDataLogAwsLogEntryToLog
  & interpretLogDataLog
  & interpretTimeGhc
  & setLogLevel (Just Info)
  & interpretDataLogToJsonStdout (logEntryToJson logMessageToJson)
  & runResource
  & embedToFinal @IO
  & runFinal @IO

reportFatal :: ()
  => Member (Embed IO) r
  => AppError
  -> Sem r ()
reportFatal (AppError msg) =
  T.putStrLn msg

runDownCmd :: CLI.DownCmd -> IO ()
runDownCmd cmd = runApp cmd do
  initialiseDb
    & trap @AWS.Error (throw . AppError . T.pack . show)
    & trap @RdsDataError (throw . AppError . T.pack . show)

  migrateDown (cmd ^. the @"migrationFp")
    & trap @AWS.Error (throw . AppError . T.pack . show)
    & trap @IOException (throw . AppError . T.pack . show)
    & trap @JsonDecodeError (throw . AppError . T.pack . show)
    & trap @RdsDataError (throw . AppError . T.pack . show)
    & trap @YamlDecodeError (throw . AppError . T.pack . show)

  id do
    res <- executeStatement
      ( mconcat
        [ "SELECT"
        , "  uuid,"
        , "  created_at,"
        , "  deployed_by"
        , "FROM migration"
        ]
      )
      & trap @AWS.Error (throw . AppError . T.pack . show)
      & trap @RdsDataError (throw . AppError . T.pack . show)

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
