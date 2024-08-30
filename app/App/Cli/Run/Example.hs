{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

{- HLINT ignore "Use let" -}

module App.Cli.Run.Example
  ( runExampleCmd
  ) where

import           Amazonka.RDSData
import           App.AWS.Env
import           App.Config
import           Control.Monad.IO.Class
import           Data.ByteString           (ByteString)
import           Data.Generics.Product.Any
import           Data.Int
import           Data.Maybe
import           Data.RdsData.Types
import           Data.Text                 (Text)
import           Data.Time
import           Data.UUID                 (UUID)
import           Data.Word
import           GHC.Generics
import           Lens.Micro

import qualified Amazonka                  as AWS
import qualified App.Cli.Types             as CLI
import qualified App.Console               as T
import qualified Data.Aeson                as J
import qualified Data.RdsData.Decode.Row   as DEC
import qualified Data.Text                 as T
import qualified Data.Text.Lazy.Encoding   as LT
import qualified Data.Text.Lazy.IO         as LT
import qualified System.IO                 as IO
import qualified System.IO.Unsafe          as IO

data ExampleRow = ExampleRow
  { theBigInt            :: Int64
  , theBigSerial         :: Int32
  , theBoolean           :: Bool
  , theByteA             :: ByteString
  , theCharacter         :: Text
  , theCharacters        :: Text
  , theVaryingCharacter  :: Text
  , theVaryingCharacters :: Text
  , theDate              :: Day
  , theDouble            :: Double
  , theInteger           :: Integer
  , theJson              :: J.Value
  , theJsonB             :: J.Value
  , theNumeric           :: Double
  , theNumerics          :: Double
  , theReal              :: Double
  , theSmallInt          :: Int16
  , theSmallSerial       :: Word32
  , theSerial            :: Word64
  , theText              :: Text
  , theTime              :: TimeOfDay
  , theTimes             :: TimeOfDay
  , theTimestamp         :: UTCTime
  , theTimestamps        :: UTCTime
  , theUuid              :: UUID
  } deriving (Eq, Show, Generic)

runExampleCmd :: CLI.ExampleCmd -> IO ()
runExampleCmd cmd = do
  let theAwsLogLevel   = cmd ^. the @"mAwsLogLevel"
  let theMHostEndpoint = cmd ^. the @"mHostEndpoint"
  let theRegion        = cmd ^. the @"region"
  let theResourceArn   = cmd ^. the @"resourceArn"
  let theSecretArn     = cmd ^. the @"secretArn"

  envAws <-
    liftIO (IO.unsafeInterleaveIO (mkEnv theRegion (awsLogger theAwsLogLevel)))
      <&> applyMHostEndpoint theMHostEndpoint

  AWS.runResourceT $ do
    sql <- pure $ T.pack $ unlines
      [ "CREATE TABLE all_types ("
      , "  the_bigint                bigint                not null,"
      , "  the_bigserial             bigserial             not null,"
      , "  the_boolean               boolean               not null,"
      , "  the_bytea                 bytea                 not null,"
      , "  the_character             character             not null,"
      , "  the_characters            character(2)          not null,"
      , "  the_varying_character     character varying     not null,"
      , "  the_varying_characters    character varying(2)  not null,"
      , "  the_date                  date                  not null,"
      , "  the_double                double precision      not null,"
      , "  the_integer               integer               not null,"
      , "  the_json                  json                  not null,"
      , "  the_jsonb                 jsonb                 not null,"
      , "  the_numeric               numeric               not null,"
      , "  the_numerics              numeric(4, 2)         not null,"
      , "  the_real                  real                  not null,"
      , "  the_smallint              smallint              not null,"
      , "  the_smallserial           smallserial           not null,"
      , "  the_serial                serial                not null,"
      , "  the_text                  text                  not null,"
      , "  the_time                  time                  not null,"
      , "  the_times                 time(2)               not null,"
      , "  the_timestamp             timestamp             not null,"
      , "  the_timestamps            timestamp(2)          not null,"
      , "  the_uuid                  uuid                  not null"
      , ")"
      ]

    req <- pure $ newExecuteStatement theResourceArn theSecretArn sql

    res <- AWS.send envAws req

    liftIO . LT.putStrLn $ LT.decodeUtf8 $ J.encode res

  AWS.runResourceT $ do
    sql <- pure $ T.pack $ unlines
      [ "SELECT"
      , "  the_bigint,"
      , "  the_bigserial,"
      , "  the_boolean,"
      , "  the_bytea,"
      , "  the_character,"
      , "  the_characters,"
      , "  the_varying_character,"
      , "  the_varying_characters,"
      , "  the_date,"
      , "  the_double,"
      , "  the_integer,"
      , "  the_json,"
      , "  the_jsonb,"
      , "  the_numeric,"
      , "  the_numerics,"
      , "  the_real,"
      , "  the_smallint,"
      , "  the_smallserial,"
      , "  the_serial,"
      , "  the_text,"
      , "  the_time,"
      , "  the_times,"
      , "  the_timestamp,"
      , "  the_timestamps,"
      , "  the_uuid"
      , "FROM all_types"
      ]

    liftIO $ T.putStrLn sql

    req <- pure $ newExecuteStatement theResourceArn theSecretArn sql

    res <- AWS.send envAws req

    liftIO . LT.putStrLn $ LT.decodeUtf8 $ J.encode res

    decodeExampleRow <- pure $ id @(DEC.DecodeRow ExampleRow) $
      ExampleRow
        <$> DEC.int64
        <*> DEC.int32
        <*> DEC.bool
        <*> DEC.bytestring
        <*> DEC.text
        <*> DEC.text
        <*> DEC.text
        <*> DEC.text
        <*> DEC.day
        <*> DEC.double
        <*> DEC.integer
        <*> DEC.json
        <*> DEC.json
        <*> DEC.double
        <*> DEC.double
        <*> DEC.double
        <*> DEC.int16
        <*> DEC.word32
        <*> DEC.word64
        <*> DEC.text
        <*> DEC.timeOfDay
        <*> DEC.timeOfDay
        <*> DEC.utcTime
        <*> DEC.utcTime
        <*> DEC.uuid

    records <- pure $ id @[[Value]] $ fromMaybe [] $ mapM (mapM fromField) =<< res ^. the @"records"

    row <- pure $ DEC.decodeRows decodeExampleRow records

    liftIO $ IO.print row

  pure ()
