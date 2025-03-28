{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant pure" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use let" -}

module Test.Data.RdsData.Migration.ConnectionSpec
  ( tasty_rds_integration_test,
  ) where

import qualified Amazonka.Types                         as AWS
import           Data.Function
import           Data.Generics.Product.Any
import qualified Data.List                              as L
import           Data.RdsData.Default
import           Data.RdsData.Polysemy.Core
import           Data.RdsData.Polysemy.Error
import           Data.RdsData.Polysemy.Migration
import           Data.RdsData.Polysemy.Test.Cluster
import           Data.RdsData.Polysemy.Test.Env
import           Data.RdsData.Polysemy.Test.Workspace
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.File
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.Prelude
import           HaskellWorks.TestContainers.LocalStack
import           Lens.Micro
import qualified System.Info                            as OS
import qualified Test.Tasty                             as Tasty
import qualified Test.Tasty.Hedgehog                    as H
import qualified TestContainers.Tasty                   as TC

isWindows :: Bool
isWindows = OS.os == "mingw32"

isMacos :: Bool
isMacos = OS.os == "darwin"

-- cabal test rds-data-test --test-options "--pattern \"/RDS integration test/\""
tasty_rds_integration_test :: Tasty.TestTree
tasty_rds_integration_test =
  if isWindows || isMacos
    then Tasty.testGroup "LocalStackSpec skipped on Windows and MacOS" []
    else
      TC.withContainers (setupContainers' projectDefaultLocalStack) $ \getContainer ->
        H.testProperty "RDS integration test" $ propertyOnce $ localWorkspace "rds-data" $ runLocalTestEnv getContainer $ do
          rdsClusterDetails <- createRdsDbCluster "rds_data_migration" getContainer

          dbClusterArn <- rdsClusterDetails ^. the @"createDbClusterResponse" . the @"dbCluster" . _Just . the @"dbClusterArn"
            & nothingFail

          runReaderStatementContextFromClusterDetails rdsClusterDetails $ do
            waitUntilRdsDbClusterAvailable dbClusterArn
              & trapFail @AWS.Error
              & jotShowDataLog @AwsLogEntry

            initialiseDb
              & trapFail @RdsDataError
              & trapFail @AWS.Error
              & jotShowDataLog @AwsLogEntry

            migrateUp "db/migration.yaml"
              & trapFail @AWS.Error
              & trapFail @IOException
              & trapFail @JsonDecodeError
              & trapFail @RdsDataError
              & trapFail @YamlDecodeError
              & jotShowDataLog

            upResult <-
              ( executeStatement $ mconcat
                  [ "SELECT table_name"
                  , "  FROM information_schema.tables"
                  , "  WHERE table_schema = 'public'"
                  , "    AND table_type = 'BASE TABLE';"
                  ]
              )
              & trapFail @AWS.Error
              & trapFail @RdsDataError
              & jotShowDataLog

            let upTables = upResult ^.. the @"records" . each . each . each . the @"stringValue" . _Just

            L.sort upTables === ["examples", "migration", "projects", "users"]

            migrateDown "db/migration.yaml"
              & trapFail @AWS.Error
              & trapFail @IOException
              & trapFail @JsonDecodeError
              & trapFail @RdsDataError
              & trapFail @YamlDecodeError
              & jotShowDataLog

            downResult <-
              ( executeStatement $ mconcat
                  [ "SELECT table_name"
                  , "  FROM information_schema.tables"
                  , "  WHERE table_schema = 'public'"
                  , "    AND table_type = 'BASE TABLE'"
                  ]
              )
              & trapFail @AWS.Error
              & trapFail @RdsDataError
              & jotShowDataLog

            let downTables = downResult ^.. the @"records" . each . each . each . the @"stringValue" . _Just

            L.sort downTables === ["migration"]
