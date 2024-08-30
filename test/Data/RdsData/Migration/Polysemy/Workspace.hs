{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Data.RdsData.Migration.Polysemy.Workspace
  ( databaseName,
    localWorkspace,
  ) where

import qualified Data.Text as Text

import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.Prelude
import           Polysemy ()

databaseName :: Text
databaseName = "rds_data_migration"

localWorkspace :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member Log r
  => Sem
        ( Reader Workspace
        : Reader ProjectRoot
        : Reader PackagePath
        : Resource
        : r)
        ()
  -> Sem r ()
localWorkspace f =
  withFrozenCallStack $ do
    cabalProjectDir <- findCabalProjectDir "."

    f & moduleWorkspace (Text.unpack databaseName)
      & runReader (ProjectRoot cabalProjectDir)
      & runReader (PackagePath "rds-data-migration")
      & runResource
