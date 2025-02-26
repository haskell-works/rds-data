{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}

module App.Cli.Run.Up
  ( runUpCmd
  ) where

import           Data.Generics.Product.Any
import           Lens.Micro

import qualified Amazonka                        as AWS
import qualified App.Cli.Types                   as CLI
import qualified App.Console                     as T
import           Data.RdsData.Aws
import           Data.RdsData.Polysemy.Core
import           Data.RdsData.Polysemy.Error
import           Data.RdsData.Polysemy.Migration
import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.File
import           HaskellWorks.Polysemy.Log
import           HaskellWorks.Prelude
import           Polysemy.Log
import           Polysemy.Time.Interpreter.Ghc

newtype AppError
  = AppError Text
  deriving (Eq, Show)

runApp :: ()
  => CLI.UpCmd
  -> Sem
      [ Reader StatementContext
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
  & runReader (cmd ^. the @"statementContext")
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

runUpCmd :: CLI.UpCmd -> IO ()
runUpCmd cmd = runApp cmd do
  initialiseDb
    & trap @AWS.Error (throw . AppError . tshow)
    & trap @RdsDataError (throw . AppError . tshow)

  migrateUp (cmd ^. the @"migrationFp")
    & trap @AWS.Error (throw . AppError . tshow)
    & trap @IOException (throw . AppError . tshow)
    & trap @JsonDecodeError (throw . AppError . tshow)
    & trap @RdsDataError (throw . AppError . tshow)
    & trap @YamlDecodeError (throw . AppError . tshow)
