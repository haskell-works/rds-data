{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}

module App.Cli.Run.LocalStack
  ( runLocalStackCmd,
    runApp,
    reportFatal,
  ) where

import           Lens.Micro

import qualified App.Cli.Types                                     as CLI
import qualified App.Console                                       as T
import qualified Control.Concurrent                                as IO
import qualified Control.Concurrent.STM                            as IO
import qualified Control.Exception                                 as IO
import qualified Control.Monad.Trans.Resource                      as IO
import qualified Control.Monad.Trans.Resource.Internal             as IO
import           Data.Acquire                                      (ReleaseType (ReleaseNormal))
import           Data.Generics.Product.Any
import           Data.RdsData.Aws
import           Data.RdsData.Default
import           Data.RdsData.Internal.Show
import           Data.RdsData.Polysemy.Test.Cluster
import           Data.RdsData.Polysemy.Test.Env
import           GHC.IORef                                         (IORef)
import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.Amazonka.LocalStack
import qualified HaskellWorks.Polysemy.Control.Concurrent.STM      as STM
import qualified HaskellWorks.Polysemy.Control.Concurrent.STM.TVar as STM
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Prelude
import           HaskellWorks.TestContainers.LocalStack
import qualified Hedgehog                                          as H
import           Polysemy.Log
import           Polysemy.Time.Interpreter.Ghc
import qualified System.Exit                                       as IO
import qualified TestContainers.Monad                              as TC
import qualified TestContainers.Tasty                              as TC

newtype AppError
  = AppError Text
  deriving (Eq, Show)

runApp :: ()
  => Sem
      [ Error AppError
      , DataLog AwsLogEntry
      , Log
      , GhcTime
      , DataLog (LogEntry LogMessage)
      , Resource
      , Embed IO
      , Final IO
      ] ()
  -> IO ()
runApp f = f
  & trap @AppError reportFatal
  & interpretDataLogAwsLogEntryToLog
  & interpretLogDataLog
  & interpretTimeGhc
  & setLogLevel (Just Info)
  & interpretDataLogStdout
  & runResource
  & embedToFinal @IO
  & runFinal @IO

reportFatal :: ()
  => Member (Embed IO) r
  => AppError
  -> Sem r ()
reportFatal (AppError msg) =
  T.putStrLn msg


startContainers :: ()
  => TC.Config
  -> IO (TC.Container, IO.InternalState)
startContainers tcConfig =
  TC.runTestContainer tcConfig do
    result <- setupContainers' projectDefaultLocalStack
    releaseMap <- IO.liftResourceT IO.getInternalState

    -- N.B. runResourceT runs the finalizers on every resource. We don't want it to! We want to run
    -- finalization in the release function that is called by Tasty! stateAlloc increments a references
    -- count to accomodate for exactly these kind of cases.
    liftIO $ IO.stateAlloc releaseMap
    pure (result, releaseMap)

stopContainers :: ()
  => (a, IORef IO.ReleaseMap)
  -> IO ()
stopContainers (_, internalState) = do
  T.putStrLn "Stopping containers"
  IO.stateCleanup ReleaseNormal internalState

runLocalStackCmd :: CLI.LocalStackCmd -> IO ()
runLocalStackCmd _ = do
  tvRunning <- IO.newTVarIO False

  tcConfig <- TC.determineConfig

  void $ IO.bracket (startContainers tcConfig) stopContainers \(container, _) -> do
    void $ H.check $ propertyOnce do
      STM.atomically $ STM.writeTVar tvRunning True

      void $ runLocalTestEnv (pure container) do
        rdsClusterDetails <- createRdsDbCluster "rds_data_migration" (pure container)

        runReaderStatementContextFromClusterDetails rdsClusterDetails do
          lsEp <- getLocalStackEndpoint container
          jotShow_ lsEp -- Localstack endpoint
          let port = lsEp ^. the @"port"
          let exampleCmd = "awslocal --endpoint-url=http://localhost:" <> tshow port <> " s3 ls"
          -- Example awslocal command:
          jot_ exampleCmd
          jotShowM_ $ ask @StatementContext
          pure ()

      failure -- Not a failure

    running <- IO.readTVarIO tvRunning

    if running
      then do
        T.putStrLn "Localstack RDS cluster is running.  Type CTRL-C to exit."
        void . forever $ IO.threadDelay 10000000
      else do
        T.putStrLn "Failed to start Localstack RDS cluster."
        IO.exitFailure
