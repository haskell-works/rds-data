{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use <&>" -}

module Data.RdsData.Decode.Array
  ( DecodeArray(..)

  , decodeArrayFailedMessage

  , arrays
  , bools
  , doubles
  , integers
  , texts

  , days
  , int16s
  , int32s
  , int64s
  , int8s
  , ints
  , jsons
  , lazyTexts
  , strings
  , timesOfDay
  , ulids
  , utcTimes
  , uuids
  , word16s
  , word32s
  , word64s
  , word8s
  , words
  ) where

import Control.Applicative
import Data.Int
import Data.RdsData.Internal.Aeson
import Data.RdsData.Types.Array
import Data.Text (Text)
import Data.Time
import Data.ULID (ULID)
import Data.UUID (UUID)
import Data.Word
import Prelude hiding (maybe, null, words)

import qualified Data.Aeson                    as J
import qualified Data.RdsData.Internal.Convert as CONV
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as LT
import qualified Data.UUID                     as UUID
import qualified Prelude                       as P

newtype DecodeArray a = DecodeArray
  { decodeArray :: Array -> Either Text a
  } deriving Functor

instance Applicative DecodeArray where
  pure a = DecodeArray \_ -> Right a
  DecodeArray f <*> DecodeArray a = DecodeArray \v -> f v <*> a v

instance Alternative DecodeArray where
  empty = DecodeArray \_ -> Left "empty"
  DecodeArray a <|> DecodeArray b = DecodeArray \v ->
    either (const (b v)) Right (a v)

instance Monad DecodeArray where
  DecodeArray a >>= f = DecodeArray \v -> do
    a' <- a v
    decodeArray (f a') v

--------------------------------------------------------------------------------

decodeArrayFailedMessage :: Text -> Text -> Maybe Text -> Array -> Text
decodeArrayFailedMessage item type_ reason value =
  mconcat
    [ "Failed to decode " <> item <> " of type " <> type_ <> " from Array of " <> toJsonText value
    , P.maybe "" (" because " <>) reason
    ]

--------------------------------------------------------------------------------

integers :: DecodeArray [Integer]
integers =
  DecodeArray \v ->
    case v of
      ArrayOfIntegers es -> Right es
      _ -> Left $ decodeArrayFailedMessage "integers" "ArrayOfIntegers" Nothing v

texts :: DecodeArray [Text]
texts =
  DecodeArray \v ->
    case v of
      ArrayOfTexts es -> Right es
      _ -> Left $ decodeArrayFailedMessage "texts" "ArrayOfStrings" Nothing v

bools :: DecodeArray [Bool]
bools =
  DecodeArray \v ->
    case v of
      ArrayOfBools es -> Right es
      _ -> Left $ decodeArrayFailedMessage "bools" "ArrayOfBooleans" Nothing v

doubles :: DecodeArray [Double]
doubles =
  DecodeArray \v ->
    case v of
      ArrayOfDoubles es -> Right es
      _ -> Left $ decodeArrayFailedMessage "doubles" "ArrayOfDoubles" Nothing v

arrays :: DecodeArray [Array]
arrays =
  DecodeArray \v ->
    case v of
      ArrayOfArrays es -> Right es
      _ -> Left $ decodeArrayFailedMessage "arrays" "ArrayOfArrays" Nothing v

--------------------------------------------------------------------------------

ints :: DecodeArray [Int]
ints =
  fmap fromIntegral <$> integers

int8s :: DecodeArray [Int8]
int8s =
  fmap fromIntegral <$> integers

int16s :: DecodeArray [Int16]
int16s =
  fmap fromIntegral <$> integers

int32s :: DecodeArray [Int32]
int32s =
  fmap fromIntegral <$> integers

int64s :: DecodeArray [Int64]
int64s =
  fmap fromIntegral <$> integers

words :: DecodeArray [Word]
words =
  fmap fromIntegral <$> integers

word8s :: DecodeArray [Word8]
word8s =
  fmap fromIntegral <$> integers

word16s :: DecodeArray [Word16]
word16s =
  fmap fromIntegral <$> integers

word32s :: DecodeArray [Word32]
word32s =
  fmap fromIntegral <$> integers

word64s :: DecodeArray [Word64]
word64s =
  fmap fromIntegral <$> integers

lazyTexts :: DecodeArray [LT.Text]
lazyTexts =
  fmap LT.fromStrict <$> texts

strings :: DecodeArray [String]
strings =
  fmap T.unpack <$> texts

jsons  :: DecodeArray [J.Value]
jsons = do
  ts <- texts
  case traverse (J.eitherDecodeStrict' . T.encodeUtf8) ts of
    Right js -> pure js
    Left e -> DecodeArray \_ -> Left $ "Failed to decode JSON: " <> T.pack e

timesOfDay :: DecodeArray [TimeOfDay]
timesOfDay = do
  ts <- texts
  case traverse (parseTimeM True defaultTimeLocale "%H:%M:%S". T.unpack) ts of
    Just tod -> pure tod
    Nothing -> DecodeArray \_ -> Left "Failed to decode TimeOfDay"

utcTimes :: DecodeArray [UTCTime]
utcTimes = do
  ts <- texts
  case traverse (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" . T.unpack) ts of
    Just utct -> pure utct
    Nothing -> DecodeArray \_ -> Left "Failed to decode UTCTime"

days :: DecodeArray [Day]
days = do
  ts <- texts
  case traverse (parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack) ts of
    Just d -> pure d
    Nothing -> DecodeArray \_ -> Left "Failed to decode Day"

-- | Decode an array of ULIDs
-- ULIDs are encoded as strings in the database and have have better database performance
-- than UUIDs stored as strings in the database.
ulids :: DecodeArray [ULID]
ulids = do
  ts <- texts
  case traverse CONV.textToUlid ts of
    Right u -> pure u
    Left msg -> DecodeArray \_ -> Left $ "Failed to decode UUID: " <> msg

uuids :: DecodeArray [UUID]
uuids = do
  ts <- texts
  case traverse (UUID.fromString . T.unpack) ts of
    Just u -> pure u
    Nothing -> DecodeArray \_ -> Left "Failed to decode UUID"
