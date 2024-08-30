{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant pure" -}
{- HLINT ignore "Use let" -}

module Test.Data.RdsData.Migration.Polysemy.Cluster
  ( RdsClusterDetails(..),
    createRdsDbCluster,
  ) where

import qualified Amazonka                                       as AWS
import qualified Amazonka.RDS                                   as AWS
import qualified Amazonka.SecretsManager                        as AWS
import           Data.Aeson                                     ((.=))
import qualified Data.Aeson                                     as J
import qualified Data.ByteString.Base64                         as B64
import qualified Data.ByteString.Lazy                           as LBS
import           Data.Function
import           Data.Generics.Product.Any
import qualified Data.Text.Encoding                             as T
import qualified Data.UUID                                      as UUID
import qualified Data.UUID.V4                                   as UUID
import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.Amazonka.LocalStack      (getLocalStackEndpoint,
                                                                 inspectContainer)
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Prelude
import           HaskellWorks.TestContainers.LocalStack
import           Lens.Micro
import           Test.Data.RdsData.Migration.Polysemy.Types     (RdsClusterDetails (RdsClusterDetails))
import           Test.Data.RdsData.Migration.Polysemy.Workspace

createRdsDbCluster :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member (Reader AWS.Env) r
  => Member Hedgehog r
  => Member Resource r
  => IO Container
  -> Sem r RdsClusterDetails
createRdsDbCluster getContainer = withFrozenCallStack do
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
