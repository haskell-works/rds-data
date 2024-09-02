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

data Cmd =
    CmdOfBatchExecuteStatementCmd BatchExecuteStatementCmd
  | CmdOfDownCmd DownCmd
  | CmdOfExampleCmd ExampleCmd
  | CmdOfExecuteStatementCmd ExecuteStatementCmd
  | CmdOfLocalStackCmd LocalStackCmd
  | CmdOfUpCmd UpCmd

data ExecuteStatementCmd = ExecuteStatementCmd
  { mAwsLogLevel  :: Maybe AWS.LogLevel
  , region        :: AWS.Region
  , mHostEndpoint :: Maybe (ByteString, Int, Bool)
  , resourceArn   :: Text
  , secretArn     :: Text
  , sql           :: Text
  } deriving Generic

data BatchExecuteStatementCmd = BatchExecuteStatementCmd
  { mAwsLogLevel  :: Maybe AWS.LogLevel
  , region        :: AWS.Region
  , mHostEndpoint :: Maybe (ByteString, Int, Bool)
  , parameterSets :: Maybe [[AWS.SqlParameter]]
  , resourceArn   :: Text
  , secretArn     :: Text
  , sql           :: Text
  } deriving Generic

data ExampleCmd = ExampleCmd
  { mAwsLogLevel  :: Maybe AWS.LogLevel
  , region        :: AWS.Region
  , mHostEndpoint :: Maybe (ByteString, Int, Bool)
  , resourceArn   :: Text
  , secretArn     :: Text
  } deriving Generic

data UpCmd = UpCmd
  { resourceArn :: Text
  , secretArn   :: Text
  , migrationFp :: FilePath
  } deriving Generic

data DownCmd = DownCmd
  { resourceArn :: Text
  , secretArn   :: Text
  , migrationFp :: FilePath
  } deriving Generic

data LocalStackCmd = LocalStackCmd
  deriving Generic
