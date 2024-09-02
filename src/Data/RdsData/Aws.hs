module Data.RdsData.Aws
  ( AwsResourceArn(..),
    AwsSecretArn(..),
  ) where

import           Data.Text (Text)

newtype AwsResourceArn = AwsResourceArn Text
  deriving (Eq, Show)

newtype AwsSecretArn = AwsSecretArn Text
  deriving (Eq, Show)
