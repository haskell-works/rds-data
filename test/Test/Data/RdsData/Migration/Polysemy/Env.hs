{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{- HLINT ignore "Redundant pure" -}
{- HLINT ignore "Use let" -}

module Test.Data.RdsData.Migration.Polysemy.Env
  ( AwsResourceArn(..),
    AwsSecretArn(..),
    runLocalTestEnv,
    runTestEnv,
    runReaderFromEnvOrFail,
    runReaderResourceAndSecretArnsFromResponses,
    newExecuteStatement,
    newBatchExecuteStatement,
  ) where

import qualified Amazonka                                   as AWS
import qualified Amazonka.RDSData                           as AWS
import           Data.Generics.Product.Any
import qualified Data.Text                                  as Text
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.Amazonka.LocalStack
import           HaskellWorks.Polysemy.Error
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.System.Environment
import           HaskellWorks.Prelude
import           HaskellWorks.TestContainers.LocalStack     (Container)
import           Lens.Micro
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Test.Data.RdsData.Migration.Polysemy.Types

data RdsDataError where
  EnvironmentVariableMissing
    :: String
    -> RdsDataError

deriving instance Show RdsDataError

newtype AwsResourceArn = AwsResourceArn Text
newtype AwsSecretArn = AwsSecretArn Text

runTestEnv :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => Sem
        ( Reader AWS.Env
        : Reader AwsResourceArn
        : Reader AwsSecretArn
        : r)
      a
  -> Sem r a
runTestEnv f =
  withFrozenCallStack $ f
    & runReaderAwsEnvDiscover
    & runReaderFromEnvOrFail (AwsResourceArn . Text.pack) "AURORA_RESOURCE_ARN"
    & runReaderFromEnvOrFail (AwsSecretArn . Text.pack) "AURORA_SECRET_ARN"

runLocalTestEnv :: ()
  => HasCallStack
  => Member (Embed IO) r
  => IO Container
  -> Sem
        ( Reader AWS.Env
        : r)
      a
  -> Sem r a
runLocalTestEnv getContainer f =
  withFrozenCallStack $ f
    & runReaderLocalAwsEnvDiscover getContainer

runReaderFromEnvOrFail :: forall i r a. ()
  => Member (Embed IO) r
  => Member Hedgehog r
  => (String -> i)
  -> String
  -> Sem (Reader i ': r) a
  -> Sem r a
runReaderFromEnvOrFail f envVar action = do
  env <- lookupEnv envVar
    & onNothingM (throw (EnvironmentVariableMissing envVar) & trapFail)

  runReader (f env) action

runReaderResourceAndSecretArnsFromResponses :: ()
  => Member Hedgehog r
  => RdsClusterDetails
  -> Sem (Reader AwsResourceArn : Reader AwsSecretArn : r) a
  -> Sem r a
runReaderResourceAndSecretArnsFromResponses details f = do
  resourceArn <- (details ^. the @"createDbClusterResponse" . the @"dbCluster" . _Just . the @"dbClusterArn")
    & nothingFail

  secretArn <- (details ^. the @"createSecretResponse" ^. the @"arn")
    & nothingFail

  f & runReader (AwsResourceArn resourceArn)
    & runReader (AwsSecretArn secretArn)

newExecuteStatement :: ()
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Text
  -> Sem r AWS.ExecuteStatement
newExecuteStatement sql = do
  AwsResourceArn theResourceArn <- ask
  AwsSecretArn theSecretArn <- ask

  pure $ AWS.newExecuteStatement theResourceArn theSecretArn sql

newBatchExecuteStatement :: ()
  => Member (Reader AwsResourceArn) r
  => Member (Reader AwsSecretArn) r
  => Text
  -> Sem r AWS.BatchExecuteStatement
newBatchExecuteStatement sql = do
  AwsResourceArn theResourceArn <- ask
  AwsSecretArn theSecretArn <- ask

  pure $ AWS.newBatchExecuteStatement theResourceArn theSecretArn sql
