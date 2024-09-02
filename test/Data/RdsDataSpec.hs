{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{- HLINT ignore "Monad law, left identity" -}
{- HLINT ignore "Use let" -}

module Data.RdsDataSpec where

import           Data.Generics.Product.Any
import           Data.Maybe
import           Data.RdsData.Internal.Aeson ()
import           Data.RdsData.Types.Value
import           Data.Time                   (TimeOfDay (..))
import           Hedgehog
import           Lens.Micro

import qualified Amazonka.RDSData            as AWS
import qualified Data.Aeson                  as J
import qualified Data.Aeson.Encode.Pretty    as J
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.RdsData.Decode.Row     as DEC
import qualified Data.RdsData.Encode.Row     as ENC
import qualified Data.RdsData.TestTypes      as TT
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Hedgehog                    as H
import qualified Hedgehog.Extras             as H
import qualified Hedgehog.Extras.Test.Golden as H

hprop_stub :: Property
hprop_stub = H.propertyOnce $ do
  H.evalIO $ pure ()
  response <- H.evalIO (J.eitherDecodeFileStrict @AWS.ExecuteStatementResponse "test/files/row.json")
    >>= either (const H.failure) pure

  records <- pure (response ^. the @"records")
    >>= maybe H.failure pure

  values <- pure $ mapMaybe fromField <$> records

  rows <- pure
    do
      DEC.decodeRows
        do
          TT.Record
            <$> DEC.int
            <*> DEC.int
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
            <*> DEC.text
            <*> DEC.text
            <*> DEC.double
            <*> DEC.int
            <*> DEC.int
            <*> DEC.int
            <*> DEC.text
            <*> DEC.timeOfDay
            <*> DEC.timeOfDay
            <*> DEC.utcTime
            <*> DEC.utcTime
            <*> DEC.ulid
            <*> DEC.uuid
        values
    >>= either (\s -> H.noteShow_ s >> H.failure) pure

  rows ===
    [ TT.Record
      { TT.bigint             = 1234567890
      , TT.bigserial          = 1
      , TT.boolean            = True
      , TT.bytea              = "EjQ="
      , TT.character          = "A"
      , TT.characters         = "AB"
      , TT.varyingCharacter   = "C"
      , TT.varyingCharacters  = "CD"
      , TT.date               = read "2024-02-04"
      , TT.double             = 3.14159265359
      , TT.integer            = 42
      , TT.json               = fromJust $ J.decode "{\"key\": \"value\"}"
      , TT.jsonb              = fromJust $ J.decode "{\"key\": \"value\"}"
      , TT.numeric            = "1234.56"
      , TT.numerics           = "12.34"
      , TT.real               = 3.14
      , TT.smallint           = 12345
      , TT.smallserial        = 1
      , TT.serial             = 1
      , TT.text               = "Some text"
      , TT.time               = TimeOfDay 12 34 56
      , TT.times              = TimeOfDay 12 34 56.78
      , TT.timestamp          = read "2024-02-04 12:34:56 UTC"
      , TT.timestamps         = read "2024-02-04 12:34:56 UTC"
      , TT.ulid               = read "0123456789ABCDEFGHJKMNPQRS"
      , TT.uuid               = read "550e8400-e29b-41d4-a716-446655440000"
      }
    ]


hprop_stub2 :: Property
hprop_stub2 = H.propertyOnce $ do
  record <- pure
    TT.Record
      { TT.bigint             = 1234567890
      , TT.bigserial          = 1
      , TT.boolean            = True
      , TT.bytea              = "EjQ="
      , TT.character          = "A"
      , TT.characters         = "AB"
      , TT.varyingCharacter   = "C"
      , TT.varyingCharacters  = "CD"
      , TT.date               = read "2024-02-04"
      , TT.double             = 3.14159265359
      , TT.integer            = 42
      , TT.json               = fromJust $ J.decode "{\"key\": \"value\"}"
      , TT.jsonb              = fromJust $ J.decode "{\"key\": \"value\"}"
      , TT.numeric            = "1234.56"
      , TT.numerics           = "12.34"
      , TT.real               = 3.14
      , TT.smallint           = 12345
      , TT.smallserial        = 1
      , TT.serial             = 1
      , TT.text               = "Some text"
      , TT.time               = TimeOfDay 12 34 56
      , TT.times              = TimeOfDay 12 34 56.78
      , TT.timestamp          = read "2024-02-04 12:34:56 UTC"
      , TT.timestamps         = read "2024-02-04 12:34:56 UTC"
      , TT.ulid               = read "0123456789ABCDEFGHJKMNPQRS"
      , TT.uuid               = read "550e8400-e29b-41d4-a716-446655440000"
      }

  row <- pure $
    mconcat
      [ ENC.encodeRow ENC.int         (record ^. the @"bigint"           ) []
      , ENC.encodeRow ENC.int         (record ^. the @"bigserial"        ) []
      , ENC.encodeRow ENC.bool        (record ^. the @"boolean"          ) []
      , ENC.encodeRow ENC.bytestring  (record ^. the @"bytea"            ) []
      , ENC.encodeRow ENC.text        (record ^. the @"character"        ) []
      , ENC.encodeRow ENC.text        (record ^. the @"characters"       ) []
      , ENC.encodeRow ENC.text        (record ^. the @"varyingCharacter" ) []
      , ENC.encodeRow ENC.text        (record ^. the @"varyingCharacters") []
      , ENC.encodeRow ENC.day         (record ^. the @"date"             ) []
      , ENC.encodeRow ENC.double      (record ^. the @"double"           ) []
      , ENC.encodeRow ENC.integer     (record ^. the @"integer"          ) []
      , ENC.encodeRow ENC.json        (record ^. the @"json"             ) []
      , ENC.encodeRow ENC.json        (record ^. the @"jsonb"            ) []
      , ENC.encodeRow ENC.text        (record ^. the @"numeric"          ) []
      , ENC.encodeRow ENC.text        (record ^. the @"numerics"         ) []
      , ENC.encodeRow ENC.double      (record ^. the @"real"             ) []
      , ENC.encodeRow ENC.int         (record ^. the @"smallint"         ) []
      , ENC.encodeRow ENC.int         (record ^. the @"smallserial"      ) []
      , ENC.encodeRow ENC.int         (record ^. the @"serial"           ) []
      , ENC.encodeRow ENC.text        (record ^. the @"text"             ) []
      , ENC.encodeRow ENC.timeOfDay   (record ^. the @"time"             ) []
      , ENC.encodeRow ENC.timeOfDay   (record ^. the @"times"            ) []
      , ENC.encodeRow ENC.utcTime     (record ^. the @"timestamp"        ) []
      , ENC.encodeRow ENC.utcTime     (record ^. the @"timestamps"       ) []
      , ENC.encodeRow ENC.uuid        (record ^. the @"uuid"             ) []
      ]

  json <- pure $ T.unpack $ T.decodeUtf8 $ LBS.toStrict $ J.encodePretty row

  H.diffVsGoldenFile json "test/files/golden/row.json"

  response <- pure $
    AWS.newExecuteStatementResponse 0
      & the @"records" .~ Just [toField <$> row]

  responseJson <- pure $ T.unpack $ T.decodeUtf8 $ LBS.toStrict $ J.encodePretty response

  H.diffVsGoldenFile responseJson "test/files/golden/rds-row.json"

  True === True

