{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.RdsData.Polysemy.Error
  ( RdsDataError(..)
  ) where

import           Data.Text (Text)

data RdsDataError where
  EnvironmentVariableMissing
    :: String
    -> RdsDataError

  RdsDataError
    :: Text
    -> RdsDataError

deriving instance Show RdsDataError
