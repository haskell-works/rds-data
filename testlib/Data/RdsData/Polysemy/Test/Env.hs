{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{- HLINT ignore "Redundant pure" -}
{- HLINT ignore "Use let" -}

module Data.RdsData.Polysemy.Test.Env
  ( AwsResourceArn(..),
    AwsSecretArn(..),
    runLocalTestEnv,
    runTestEnv,
    runReaderFromEnvOrFail,
    runReaderStatementContextFromClusterDetails,
  ) where

import qualified Amazonka                                  as AWS
import           Data.Generics.Product.Any
import           Data.RdsData.Aws
import           Data.RdsData.Migration.Types
import           Data.RdsData.Polysemy.Error
import qualified Data.Text                                 as Text
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.Amazonka.LocalStack
import           HaskellWorks.Polysemy.Error
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.System.Environment
import           HaskellWorks.Prelude
import           HaskellWorks.TestContainers.LocalStack    (Container)
import           Lens.Micro
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

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

runReaderStatementContextFromClusterDetails :: ()
  => Member Hedgehog r
  => RdsClusterDetails
  -> Sem (Reader StatementContext : r) a
  -> Sem r a
runReaderStatementContextFromClusterDetails details f = do
  resourceArn <- (details ^. the @"createDbClusterResponse" . the @"dbCluster" . _Just . the @"dbClusterArn")
    & nothingFail

  secretArn <- (details ^. the @"createSecretResponse" ^. the @"arn")
    & nothingFail

  mDatabase <- pure $ (details ^? the @"createDbClusterResponse" . the @"dbCluster" . _Just . the @"databaseName" . _Just)
    <&> Database

  statementContext <- pure $ newStatementContext (AwsResourceArn resourceArn) (AwsSecretArn secretArn)
    & the @"database" .~ mDatabase


  f & runReader statementContext
