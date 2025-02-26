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
import           Data.ByteString            (ByteString)
import           Data.Generics.Product.Any
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.RdsData.Internal.Show
import           Data.RdsData.Types
import           Data.Text                  (Text)
import           Data.Time
import           Data.UUID                  (UUID)
import           Data.Word
import           GHC.Generics
import           Lens.Micro
import           Text.Printf

import qualified Amazonka                   as AWS
import qualified App.Cli.Types              as CLI
import qualified App.Console                as T
import qualified Data.Aeson                 as J
import qualified Data.RdsData.Decode.Row    as DEC
import qualified Data.RdsData.Encode.Params as ENC
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy.Encoding    as LT
import qualified Data.Text.Lazy.IO          as LT
import qualified System.IO                  as IO
import qualified System.IO.Unsafe           as IO

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
  let theResourceArn   = cmd ^. the @"statementContext" . the @"resourceArn" . the @1
  let theSecretArn     = cmd ^. the @"statementContext" . the @"secretArn" . the @1

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
      [ "INSERT INTO all_types ("
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
      , ") VALUES ("
      , "  :p00," -- the_bigint
      , "  :p01," -- the_bigserial
      , "  :p02," -- the_boolean
      , "  :p03," -- the_bytea
      , "  :p04," -- the_character
      , "  :p05," -- the_characters
      , "  :p06," -- the_varying_character
      , "  :p07," -- the_varying_characters
      , "  :p08," -- the_date
      , "  :p09," -- the_double
      , "  :p10," -- the_integer
      , "  :p11," -- the_json
      , "  :p12," -- the_jsonb
      , "  :p13," -- the_numeric
      , "  :p14," -- the_numerics
      , "  :p15," -- the_real
      , "  :p16," -- the_smallint
      , "  :p17," -- the_smallserial
      , "  :p18," -- the_serial
      , "  :p19," -- the_text
      , "  :p20," -- the_time
      , "  :p21," -- the_times
      , "  :p22," -- the_timestamp
      , "  :p23," -- the_timestamps
      , "  :p24"  -- the_uuid
      , ")"
      ]

    exampleRow <- pure $ ExampleRow
      { theBigInt             = 1234567890
      , theBigSerial          = 1
      , theBoolean            = True
      , theByteA              = T.encodeUtf8 "EjQ="
      , theCharacter          = "A"
      , theCharacters         = "AB"
      , theVaryingCharacter   = "C"
      , theVaryingCharacters  = "CD"
      , theDate               = YearMonthDay 2024 02 04
      , theDouble             = 3.14159265359
      , theInteger            = 42
      , theJson               = "{\"key\":\"value\"}"
      , theJsonB              = "{\"key\":\"value\"}"
      , theNumeric            = 1234.56
      , theNumerics           = 12.34
      , theReal               = 3.14
      , theSmallInt           = 12345
      , theSmallSerial        = 1
      , theSerial             = 1
      , theText               = "Some text"
      , theTime               = TimeOfDay 12 34 56
      , theTimes              = TimeOfDay 12 34 56.780
      , theTimestamp          = read "2024-02-04 12:34:56"
      , theTimestamps         = read "2024-02-04 12:34:56"
      , theUuid               = read "550e8400-e29b-41d4-a716-446655440000"
      }

    row <- pure $ flip appEndo [] $ mconcat $ fmap Endo
      [ ENC.encodeParams ENC.int64       (exampleRow ^. the @"theBigInt"            )
      , ENC.encodeParams ENC.int32       (exampleRow ^. the @"theBigSerial"         )
      , ENC.encodeParams ENC.bool        (exampleRow ^. the @"theBoolean"           )
      , ENC.encodeParams ENC.bytestring  (exampleRow ^. the @"theByteA"             )
      , ENC.encodeParams ENC.text        (exampleRow ^. the @"theCharacter"         )
      , ENC.encodeParams ENC.text        (exampleRow ^. the @"theCharacters"        )
      , ENC.encodeParams ENC.text        (exampleRow ^. the @"theVaryingCharacter"  )
      , ENC.encodeParams ENC.text        (exampleRow ^. the @"theVaryingCharacters" )
      , ENC.encodeParams ENC.day         (exampleRow ^. the @"theDate"              )
      , ENC.encodeParams ENC.double      (exampleRow ^. the @"theDouble"            )
      , ENC.encodeParams ENC.integer     (exampleRow ^. the @"theInteger"           )
      , ENC.encodeParams ENC.json        (exampleRow ^. the @"theJson"              )
      , ENC.encodeParams ENC.json        (exampleRow ^. the @"theJsonB"             )
      , ENC.encodeParams ENC.double      (exampleRow ^. the @"theNumeric"           )
      , ENC.encodeParams ENC.double      (exampleRow ^. the @"theNumerics"          )
      , ENC.encodeParams ENC.double      (exampleRow ^. the @"theReal"              )
      , ENC.encodeParams ENC.int16       (exampleRow ^. the @"theSmallInt"          )
      , ENC.encodeParams ENC.word32      (exampleRow ^. the @"theSmallSerial"       )
      , ENC.encodeParams ENC.word64      (exampleRow ^. the @"theSerial"            )
      , ENC.encodeParams ENC.text        (exampleRow ^. the @"theText"              )
      , ENC.encodeParams ENC.timeOfDay   (exampleRow ^. the @"theTime"              )
      , ENC.encodeParams ENC.timeOfDay   (exampleRow ^. the @"theTimes"             )
      , ENC.encodeParams ENC.utcTime     (exampleRow ^. the @"theTimestamp"         )
      , ENC.encodeParams ENC.utcTime     (exampleRow ^. the @"theTimestamps"        )
      , ENC.encodeParams ENC.uuid        (exampleRow ^. the @"theUuid"              )
      ]

    liftIO $ LT.putStrLn $ LT.decodeUtf8 $ J.encode row

    req <- pure $ newBatchExecuteStatement theResourceArn theSecretArn sql
      & the @"parameterSets" ?~
        [ zip row [0..] <&>
          ( \(v, i) ->
              toSqlParameter v
                & the @"name" ?~ T.pack ("p" <> printf "%02d" (id @Int i))
          )
        ]

    liftIO $ T.putStrLn $ "===> " <> tshow req

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
