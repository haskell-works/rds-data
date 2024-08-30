{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Cli.Types
  ( Cmd(..)
  , DescribeCmd(..)
  , ExecuteStatementCmd(..)
  , ExampleCmd(..)
  ) where

import           Data.ByteString    (ByteString)
import           Data.RdsData.Types ()
import           Data.Text
import           GHC.Generics

import qualified Amazonka           as AWS

data Cmd =
    CmdOfDescribeCmd DescribeCmd
  | CmdOfExecuteStatementCmd ExecuteStatementCmd
  | CmdOfExampleCmd ExampleCmd

data DescribeCmd = DescribeCmd
  { mAwsLogLevel  :: Maybe AWS.LogLevel
  , region        :: AWS.Region
  , mHostEndpoint :: Maybe (ByteString, Int, Bool)
  , resourceArn   :: Text
  , secretArn     :: Text
  } deriving Generic

data ExecuteStatementCmd = ExecuteStatementCmd
  { mAwsLogLevel  :: Maybe AWS.LogLevel
  , region        :: AWS.Region
  , mHostEndpoint :: Maybe (ByteString, Int, Bool)
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
