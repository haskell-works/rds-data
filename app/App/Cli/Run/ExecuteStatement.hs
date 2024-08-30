{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module App.Cli.Run.ExecuteStatement
  ( runExecuteStatementCmd
  ) where

import Amazonka.RDSData
import App.AWS.Env
import App.Config
import Control.Monad.IO.Class
import Data.Generics.Product.Any
import Lens.Micro

import qualified App.Cli.Types              as CLI
import qualified Amazonka                   as AWS
import qualified Data.Aeson                 as J
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T
import qualified System.IO.Unsafe           as IO

runExecuteStatementCmd :: CLI.ExecuteStatementCmd -> IO ()
runExecuteStatementCmd cmd = do
  let theAwsLogLevel   = cmd ^. the @"mAwsLogLevel"
  let theMHostEndpoint = cmd ^. the @"mHostEndpoint"
  let theRegion        = cmd ^. the @"region"
  let theResourceArn   = cmd ^. the @"resourceArn"
  let theSecretArn     = cmd ^. the @"secretArn"
  let theSql           = cmd ^. the @"sql"

  envAws <-
    liftIO (IO.unsafeInterleaveIO (mkEnv theRegion (awsLogger theAwsLogLevel)))
      <&> applyMHostEndpoint theMHostEndpoint

  let req = newExecuteStatement theResourceArn theSecretArn theSql

  AWS.runResourceT $ do
    res <- AWS.send envAws req

    liftIO . T.putStrLn $ T.decodeUtf8 $ LBS.toStrict $ J.encode res
