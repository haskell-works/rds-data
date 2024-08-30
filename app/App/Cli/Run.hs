{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module App.Cli.Run
  ( runCmd
  ) where

import App.Cli.Run.Describe
import App.Cli.Run.Example
import App.Cli.Run.ExecuteStatement

import qualified App.Cli.Types as CLI

runCmd :: CLI.Cmd -> IO ()
runCmd = \case
  CLI.CmdOfDescribeCmd cmd ->
    runDescribeCmd cmd
  CLI.CmdOfExecuteStatementCmd cmd ->
    runExecuteStatementCmd cmd
  CLI.CmdOfExampleCmd cmd ->
    runExampleCmd cmd
