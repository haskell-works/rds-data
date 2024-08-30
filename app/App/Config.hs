{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module App.Config
  ( applyMHostEndpoint
  ) where

import Lens.Micro
import Data.ByteString (ByteString)
import Data.Generics.Product.Any

import qualified Amazonka as AWS

applyMHostEndpoint :: Maybe (ByteString, Int, Bool) -> AWS.Env -> AWS.Env
applyMHostEndpoint = \case
  Just (host, port, ssl) ->
    \env ->
      env
        & the @"overrides" .~ \svc ->
            svc & the @"endpoint" %~ \mkEndpoint region ->
              mkEndpoint region
                & the @"host"   .~ host
                & the @"port"   .~ port
                & the @"secure" .~ ssl
  Nothing -> id
