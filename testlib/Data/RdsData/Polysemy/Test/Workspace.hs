{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.RdsData.Polysemy.Test.Workspace
  ( localWorkspace,
  ) where

import qualified Data.Text                      as T

import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.Prelude
import           Polysemy                       ()

localWorkspace :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member Log r
  => Text
  -> Sem
        ( Reader Workspace
        : Reader ProjectRoot
        : Reader PackagePath
        : Resource
        : r)
        ()
  -> Sem r ()
localWorkspace prefix f =
  withFrozenCallStack $ do
    cabalProjectDir <- findCabalProjectDir "."

    f & moduleWorkspace (T.unpack prefix)
      & runReader (ProjectRoot cabalProjectDir)
      & runReader (PackagePath "rds-data")
      & runResource
