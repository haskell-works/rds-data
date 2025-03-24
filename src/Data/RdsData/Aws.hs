{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.RdsData.Aws
  ( AwsResourceArn(AwsResourceArn),
    AwsSecretArn(AwsSecretArn),
    Database(..),
    StatementContext(StatementContext),
    newStatementContext,
  ) where

import           Data.String  (IsString)
import           Data.Text    (Text)
import           GHC.Generics

newtype AwsResourceArn = AwsResourceArn Text
  deriving (Eq, Generic, IsString, Show)

newtype AwsSecretArn = AwsSecretArn Text
  deriving (Eq, Generic, IsString, Show)

newtype Database = Database { unDatabase :: Text }
  deriving (Eq, Generic, IsString, Show)

data StatementContext = StatementContext
  { resourceArn :: AwsResourceArn
  , secretArn   :: AwsSecretArn
  , database    :: Maybe Database
  } deriving (Eq, Generic, Show)

newStatementContext :: AwsResourceArn -> AwsSecretArn -> StatementContext
newStatementContext theResourceArn theSecretArn =
  StatementContext theResourceArn theSecretArn Nothing
