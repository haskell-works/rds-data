{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.RdsData.Internal.Convert
  ( dayToText
  , jsonToText
  , timeOfDayToText
  , ulidToText
  , uuidToText
  , utcTimeToText
  , textToDouble
  , textToUlid
  ) where

import Data.Bifunctor
import Data.ULID (ULID)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time
    ( formatTime, defaultTimeLocale, Day, UTCTime, TimeOfDay )
import Prelude hiding (maybe, null)
import Text.Read

import qualified Data.Aeson           as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.ULID            as ULID
import qualified Data.ULID.Base32     as ULID
import qualified Data.UUID            as UUID

timeOfDayToText :: TimeOfDay -> Text
timeOfDayToText = 
  T.pack . formatTime defaultTimeLocale "%H:%M:%S%Q"

dayToText :: Day -> Text
dayToText = 
  T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

jsonToText :: J.Value -> Text
jsonToText =
  T.decodeUtf8 . LBS.toStrict . J.encode

utcTimeToText :: UTCTime -> Text
utcTimeToText = 
  T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

ulidToText :: ULID -> Text
ulidToText = 
  ULID.encode 26 . ULID.ulidToInteger

textToUlid :: Text -> Either Text ULID
textToUlid t =
  bimap (const ("Unable to decode ULID: " <> t)) id (readEither @ULID (T.unpack t))

uuidToText :: UUID -> Text
uuidToText = 
  T.pack . UUID.toString

textToDouble :: Text -> Maybe Double
textToDouble text =
  case reads (T.unpack text) of
    [(x, "")] -> Just x
    _         -> Nothing
