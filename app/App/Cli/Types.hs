{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Cli.Types
  ( Cmd(..)
  , BatchExecuteStatementCmd(..)
  , DownCmd(..)
  , ExampleCmd(..)
  , ExecuteStatementCmd(..)
  , LocalStackCmd(..)
  , UpCmd(..)
  ) where

import           Data.ByteString    (ByteString)
import           Data.RdsData.Types ()
import           Data.Text
import           GHC.Generics

import qualified Amazonka           as AWS
import qualified Amazonka.RDSData   as AWS
import           Data.RdsData.Aws

data Cmd =
    CmdOfBatchExecuteStatementCmd BatchExecuteStatementCmd
  | CmdOfDownCmd DownCmd
  | CmdOfExampleCmd ExampleCmd
  | CmdOfExecuteStatementCmd ExecuteStatementCmd
  | CmdOfLocalStackCmd LocalStackCmd
  | CmdOfUpCmd UpCmd

data ExecuteStatementCmd = ExecuteStatementCmd
  { mAwsLogLevel     :: Maybe AWS.LogLevel
  , region           :: AWS.Region
  , mHostEndpoint    :: Maybe (ByteString, Int, Bool)
  , statementContext :: StatementContext
  , sql              :: Text
  } deriving Generic

data BatchExecuteStatementCmd = BatchExecuteStatementCmd
  { mAwsLogLevel     :: Maybe AWS.LogLevel
  , region           :: AWS.Region
  , mHostEndpoint    :: Maybe (ByteString, Int, Bool)
  , parameterSets    :: Maybe [[AWS.SqlParameter]]
  , statementContext :: StatementContext
  , sql              :: Text
  } deriving Generic

data ExampleCmd = ExampleCmd
  { mAwsLogLevel     :: Maybe AWS.LogLevel
  , region           :: AWS.Region
  , mHostEndpoint    :: Maybe (ByteString, Int, Bool)
  , statementContext :: StatementContext
  } deriving Generic

data UpCmd = UpCmd
  { statementContext :: StatementContext
  , migrationFp      :: FilePath
  } deriving Generic

data DownCmd = DownCmd
  { statementContext :: StatementContext
  , migrationFp      :: FilePath
  } deriving Generic

data LocalStackCmd = LocalStackCmd
  deriving Generic
