{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module App.Cli.Options where

import           App.Options
import           Control.Applicative
import           Data.ByteString     (ByteString)

import qualified Amazonka.Data       as AWS
import qualified App.Cli.Types       as CLI
import           Data.RdsData.Aws
import qualified Data.Text           as T
import qualified Options.Applicative as OA

opts :: OA.ParserInfo CLI.Cmd
opts = OA.info (pCmds <**> OA.helper) $ mconcat
  [ OA.fullDesc
  , OA.header $ mconcat
    [ "rds-data"
    ]
  ]

pCmds :: OA.Parser CLI.Cmd
pCmds =
  asum
    [ subParser "up"
        $ OA.info (CLI.CmdOfUpCmd <$> pUpCmd)
        $ OA.progDesc "Up command."
    , subParser "down"
        $ OA.info (CLI.CmdOfDownCmd <$> pDownCmd)
        $ OA.progDesc "Down command."
    , subParser "local-stack"
        $ OA.info (CLI.CmdOfLocalStackCmd <$> pLocalStackCmd)
        $ OA.progDesc "Launch a local-stack RDS cluster."
    ]

pStatementContext :: OA.Parser StatementContext
pStatementContext =
  StatementContext
    <$> do  OA.strOption $ mconcat
              [ OA.long "resource-arn"
              , OA.help "Resource ARN"
              , OA.metavar "ARN"
              ]
    <*> do  OA.strOption $ mconcat
              [ OA.long "secret-arn"
              , OA.help "Secret ARN"
              , OA.metavar "ARN"
              ]
    <*> do  optional
              ( OA.strOption $ mconcat
                [ OA.long "database"
                , OA.help "Database"
                , OA.metavar "DATABASE"
                ]
              )
pUpCmd :: OA.Parser CLI.UpCmd
pUpCmd =
  CLI.UpCmd
    <$> pStatementContext
    <*> do  OA.strOption $ mconcat
              [ OA.long "migration-file"
              , OA.help "Migration File"
              , OA.metavar "FILE"
              ]

pDownCmd :: OA.Parser CLI.DownCmd
pDownCmd =
  CLI.DownCmd
    <$> pStatementContext
    <*> do  OA.strOption $ mconcat
              [ OA.long "migration-file"
              , OA.help "Migration File"
              , OA.metavar "FILE"
              ]

pLocalStackCmd :: OA.Parser CLI.LocalStackCmd
pLocalStackCmd =
  pure CLI.LocalStackCmd

parseEndpoint :: OA.Parser (ByteString, Int, Bool)
parseEndpoint =
  (,,)
    <$> do  OA.option (OA.eitherReader (AWS.fromText . T.pack)) $ mconcat
              [ OA.long "host-name-override"
              , OA.help "Override the host name (default: s3.amazonaws.com)"
              , OA.metavar "HOST_NAME"
              ]
    <*> do  OA.option OA.auto $ mconcat
              [ OA.long "host-port-override"
              , OA.help "Override the host port"
              , OA.metavar "HOST_PORT"
              ]
    <*> do  OA.option OA.auto $ mconcat
              [ OA.long "host-ssl-override"
              , OA.help "Override the host SSL"
              , OA.metavar "HOST_SSL"
              ]
