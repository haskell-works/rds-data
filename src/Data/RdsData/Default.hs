{-# LANGUAGE OverloadedStrings #-}

module Data.RdsData.Default
  ( projectDefaultLocalStack
  ) where

import           Data.Text

projectDefaultLocalStack :: Text
projectDefaultLocalStack = "localstack/localstack-pro:3.7.2"