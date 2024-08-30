{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Cli.Options where

import App.Options
import Control.Applicative
import Data.ByteString (ByteString)

import qualified App.Cli.Types              as CLI
import qualified Amazonka                   as AWS
import qualified Amazonka.Data              as AWS
import qualified Data.Text                  as T
import qualified Options.Applicative        as OA

opts :: OA.ParserInfo CLI.Cmd
opts = OA.info (pCmds <**> OA.helper) $ mconcat
  [ OA.fullDesc
  , OA.header $ mconcat
    [ "rds-data-codecs"
    ]
  ]

pCmds :: OA.Parser CLI.Cmd
pCmds =
  asum
    [ subParser "execute-statement"
        $ OA.info (CLI.CmdOfExecuteStatementCmd <$> pExecuteStatementCmd)
        $ OA.progDesc "Execute statement command."
    , subParser "example"
        $ OA.info (CLI.CmdOfExampleCmd <$> pExampleCmd)
        $ OA.progDesc "Example command."
    ]

pExecuteStatementCmd :: OA.Parser CLI.ExecuteStatementCmd
pExecuteStatementCmd =
  CLI.ExecuteStatementCmd
    <$> do  optional $ OA.option (OA.eitherReader (AWS.fromText . T.pack)) $ mconcat
              [ OA.long "aws-log-level"
              , OA.help "AWS Log Level.  One of (Error, Info, Debug, Trace)"
              , OA.metavar "AWS_LOG_LEVEL"
              ]
    <*> do  OA.option (OA.auto <|> text) $ mconcat
              [ OA.long "region"
              , OA.metavar "AWS_REGION"
              , OA.showDefault
              , OA.value AWS.Oregon
              , OA.help "The AWS region in which to operate"
              ]
    <*> do  optional parseEndpoint
    <*> do  OA.strOption $ mconcat
              [ OA.long "resource-arn"
              , OA.help "Resource ARN"
              , OA.metavar "ARN"
              ]
    <*> do  OA.strOption $ mconcat
              [ OA.long "secret-arn"
              , OA.help "Secret ARN"
              , OA.metavar "ARN"
              ]
    <*> do  OA.strOption $ mconcat
              [ OA.long "sql"
              , OA.help "SQL query"
              , OA.metavar "SQL"
              ]

pExampleCmd :: OA.Parser CLI.ExampleCmd
pExampleCmd =
  CLI.ExampleCmd
    <$> do  optional $ OA.option (OA.eitherReader (AWS.fromText . T.pack)) $ mconcat
              [ OA.long "aws-log-level"
              , OA.help "AWS Log Level.  One of (Error, Info, Debug, Trace)"
              , OA.metavar "AWS_LOG_LEVEL"
              ]
    <*> do  OA.option (OA.auto <|> text) $ mconcat
              [ OA.long "region"
              , OA.metavar "AWS_REGION"
              , OA.showDefault
              , OA.value AWS.Oregon
              , OA.help "The AWS region in which to operate"
              ]
    <*> do  optional parseEndpoint
    <*> do  OA.strOption $ mconcat
              [ OA.long "resource-arn"
              , OA.help "Resource ARN"
              , OA.metavar "ARN"
              ]
    <*> do  OA.strOption $ mconcat
              [ OA.long "secret-arn"
              , OA.help "Secret ARN"
              , OA.metavar "ARN"
              ]

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