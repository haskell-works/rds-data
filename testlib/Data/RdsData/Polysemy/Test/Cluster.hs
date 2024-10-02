{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant pure" -}
{- HLINT ignore "Use let" -}

module Data.RdsData.Polysemy.Test.Cluster
  ( RdsClusterDetails(..),
    createRdsDbCluster,
    waitUntilRdsDbClusterAvailable,
  ) where

import qualified Amazonka                                  as AWS
import qualified Amazonka.RDS                              as AWS
import qualified Amazonka.SecretsManager                   as AWS
import           Data.Aeson                                ((.=))
import qualified Data.Aeson                                as J
import qualified Data.ByteString.Base64                    as B64
import qualified Data.ByteString.Lazy                      as LBS
import           Data.Function
import           Data.Generics.Product.Any
import           Data.RdsData.Migration.Types              (RdsClusterDetails (RdsClusterDetails))
import qualified Data.Text.Encoding                        as T
import qualified Data.UUID                                 as UUID
import qualified Data.UUID.V4                              as UUID
import           HaskellWorks.Control.Monad
import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.Amazonka.LocalStack (getLocalStackEndpoint,
                                                            inspectContainer)
import           HaskellWorks.Polysemy.Control.Concurrent
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.Prelude
import           HaskellWorks.TestContainers.LocalStack
import           Lens.Micro

createRdsDbCluster :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member (Reader AWS.Env) r
  => Member Hedgehog r
  => Member Resource r
  => Text
  -> IO Container
  -> Sem r RdsClusterDetails
createRdsDbCluster databaseName getContainer = withFrozenCallStack do
  container <- embed getContainer
  jotShowM_ $ getLocalStackEndpoint container
  jotYamlM_ $ inspectContainer container
  masterUsername <- pure "masterUsername"
  masterPassword <- pure "masterPassword"

  let dbClusterId = "my-cluster"

  createDbClusterRequest <-
    pure $
      AWS.newCreateDBCluster dbClusterId "aurora-postgresql"
        & the @"masterUsername" .~ Just masterUsername
        & the @"masterUserPassword" .~ Just masterPassword
        & the @"enableHttpEndpoint" .~ Just True
        & the @"databaseName" .~ Just databaseName

  createDbClusterResponse <-
    sendAws createDbClusterRequest
      & jotShowDataLog @AwsLogEntry
      & trapFail

  let secretName = "my-aurora-cluster"

  secretString <-
    jotYaml $
      J.object
        [ "engine" .= id @Text "aurora-postgresql"
        , "username" .= id @Text masterUsername
        , "password" .= id @Text masterPassword
        , "host" .= id @Text "localhost"
        , "dbname" .= id @Text databaseName
        , "port" .= id @Text "4510"
        ]

  uuid <- embed UUID.nextRandom

  let clientRequestToken = T.encodeUtf8 $ UUID.toText uuid

  let secretStringText = T.decodeUtf8 $ LBS.toStrict $ J.encode secretString

  createSecretReq <-
    pure $
      AWS.newCreateSecret secretName
        & the @"secretString" ?~ AWS.Sensitive secretStringText
        & the @"clientRequestToken" ?~ T.decodeUtf8 (B64.encode clientRequestToken)

  createSecetResp <-
    sendAws createSecretReq
      & jotShowDataLog @AwsLogEntry
      & trapFail

  createDbInstanceReq <-
    pure $
      AWS.newCreateDBInstance dbClusterId "my-db-instance" "db.t3.medium"
        & the @"engine" .~ "aurora-postgresql"

  _dbInstanceIdResp <-
    jotShowM $
      sendAws createDbInstanceReq
        & jotShowDataLog @AwsLogEntry
        & trapFail

  pure (RdsClusterDetails createDbClusterResponse createSecetResp)

waitUntilRdsDbClusterAvailable :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member (Error AWS.Error) r
  => Member (Reader AWS.Env) r
  => Member (DataLog AwsLogEntry) r
  => Member Resource r
  => Text
  -> Sem r ()
waitUntilRdsDbClusterAvailable dbClusterArn =
  withFrozenCallStack do
    repeatNWhileM_ 120 $ \_ -> do
      result <- sendAws $
        AWS.newDescribeDBClusters
          & the @"dbClusterIdentifier" .~ Just dbClusterArn

      let mStatus = result ^? the @"dbClusters" . _Just . each . the @"status" . _Just

      if mStatus == Just "available"
        then threadDelay 1_000_000 >> pure False
        else threadDelay 1_000_000 >> pure True
