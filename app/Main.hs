{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import App.Options

import qualified App.Cli.Run          as CLI
import qualified App.Cli.Options      as CLI
import qualified Options.Applicative  as OA

main :: IO ()
main = do
  cmd <- OA.customExecParser pref CLI.opts

  CLI.runCmd cmd
