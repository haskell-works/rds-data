{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}

module App.Cli.Run
  ( runCmd
  ) where

import           App.Cli.Run.BatchExecuteStatement
import           App.Cli.Run.Down
import           App.Cli.Run.Example
import           App.Cli.Run.ExecuteStatement
import           App.Cli.Run.LocalStack
import           App.Cli.Run.Up

import qualified App.Cli.Types                     as CLI

runCmd :: CLI.Cmd -> IO ()
runCmd = \case
  CLI.CmdOfBatchExecuteStatementCmd cmd -> runBatchExecuteStatementCmd cmd
  CLI.CmdOfDownCmd cmd                  -> runDownCmd cmd
  CLI.CmdOfExampleCmd cmd               -> runExampleCmd cmd
  CLI.CmdOfExecuteStatementCmd cmd      -> runExecuteStatementCmd cmd
  CLI.CmdOfLocalStackCmd cmd            -> runLocalStackCmd cmd
  CLI.CmdOfUpCmd cmd                    -> runUpCmd cmd
