{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use <&>" -}

module Data.RdsData.Decode.Value
  ( DecodeValue(..)

  , rdsValue
  , decodeValueFailedMessage
  , decodeValueFailed
  , maybe
  , array
  , base64
  , bool
  , double
  , text
  , integer
  , null
  , int
  , int8
  , int16
  , int32
  , int64
  , word
  , word8
  , word16
  , word32
  , word64
  , bytestring
  , lazyText
  , lazyBytestring
  , string
  , json
  , timeOfDay
  , utcTime
  , uuid
  , day

  ) where

import Amazonka.Data.Base64
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Int
import Data.RdsData.Decode.Array (DecodeArray(..))
import Data.RdsData.Internal.Aeson
import Data.RdsData.Types.Value
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import Data.Word
import Prelude hiding (maybe, null)

import qualified Amazonka.Data.ByteString       as AWS
import qualified Data.Aeson                     as J
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.RdsData.Internal.Convert  as CONV
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.Lazy                 as LT
import qualified Data.UUID                      as UUID
import qualified Prelude                        as P

newtype DecodeValue a = DecodeValue
  { decodeValue :: Value -> Either Text a
  } deriving Functor

instance Applicative DecodeValue where
  pure a = DecodeValue \_ -> Right a
  DecodeValue f <*> DecodeValue a = DecodeValue \v -> f v <*> a v

instance Alternative DecodeValue where
  empty = DecodeValue \_ -> Left "empty"
  DecodeValue a <|> DecodeValue b = DecodeValue \v ->
    either (const (b v)) Right (a v)

instance Monad DecodeValue where
  DecodeValue a >>= f = DecodeValue \v -> do
    a' <- a v
    decodeValue (f a') v

--------------------------------------------------------------------------------

rdsValue :: DecodeValue Value
rdsValue =
  DecodeValue Right

decodeValueFailedMessage :: Text -> Text -> Maybe Text -> Value -> Text
decodeValueFailedMessage item type_ reason value =
  mconcat
    [ "Failed to decode " <> item <> " of type " <> type_ <> " from Value of " <> toJsonText value
    , P.maybe "" (" because " <>) reason
    ]

decodeValueFailed :: Text -> Text -> Maybe Text -> DecodeValue a
decodeValueFailed value type_ reason =
  DecodeValue $ Left . decodeValueFailedMessage value type_ reason

--------------------------------------------------------------------------------

maybe :: DecodeValue a -> DecodeValue (Maybe a)
maybe (DecodeValue f) =
  DecodeValue \v ->
    case v of
      ValueOfNull -> Right Nothing
      _ -> Just <$> f v  

--------------------------------------------------------------------------------

array :: DecodeArray a -> DecodeValue a
array decoder =
  DecodeValue \v ->
    case v of
      ValueOfArray a -> decodeArray decoder a
      _ -> Left $ decodeValueFailedMessage "array" "Array" Nothing v

base64 :: DecodeValue Base64
base64 =
  DecodeValue \v ->
    case v of
      ValueOfBase64 b64 -> Right b64
      _ -> Left $ decodeValueFailedMessage "base64" "Base64" Nothing v

bool :: DecodeValue Bool
bool =
  DecodeValue \v ->
    case v of
      ValueOfBool b -> Right b
      _ -> Left $ decodeValueFailedMessage "bool" "Bool" Nothing v

double :: DecodeValue Double
double =
  DecodeValue \v ->
    case v of
      ValueOfDouble n -> Right n
      ValueOfText t ->
        case CONV.textToDouble t of
          Just n -> Right n
          Nothing -> Left $ decodeValueFailedMessage "double" "Double" (Just "failed to parse text as double") v
      _ -> Left $ decodeValueFailedMessage "double" "Double" Nothing v

text :: DecodeValue Text
text =
  DecodeValue \v ->
    case v of
      ValueOfText s -> Right s
      _ -> Left $ decodeValueFailedMessage "text" "Text" Nothing v

integer :: DecodeValue Integer
integer =
  DecodeValue \v ->
    case v of
      ValueOfInteger n -> Right n
      _ -> Left $ decodeValueFailedMessage "integer" "Integer" Nothing v

null :: DecodeValue ()
null =
  DecodeValue \v ->
    case v of
      ValueOfNull -> Right ()
      _ -> Left $ decodeValueFailedMessage "null" "()" Nothing v

--------------------------------------------------------------------------------

int :: DecodeValue Int
int =
  asum
    [ fromIntegral <$> int64
    , decodeValueFailed "int" "Int" Nothing
    ]

int8 :: DecodeValue Int8
int8 =
  asum
    [ fromIntegral <$> integer
    , decodeValueFailed "int8" "Int8" Nothing
    ]

int16 :: DecodeValue Int16
int16 =
  asum
    [ fromIntegral <$> integer
    , decodeValueFailed "int16" "Int16" Nothing
    ]

int32 :: DecodeValue Int32
int32 =
  asum
    [ fromIntegral <$> integer
    , decodeValueFailed "int32" "Int32" Nothing
    ]

int64 :: DecodeValue Int64
int64 =
  asum
    [ fromIntegral <$> integer
    , decodeValueFailed "int64" "Int64" Nothing
    ]

word :: DecodeValue Word
word =
  asum
    [ fromIntegral <$> integer
    , decodeValueFailed "word" "Word" Nothing
    ]

word8 :: DecodeValue Word8
word8 =
  asum
    [ fromIntegral <$> integer
    , decodeValueFailed "word8" "Word8" Nothing
    ]

word16 :: DecodeValue Word16
word16 =
  asum
    [ fromIntegral <$> integer
    , decodeValueFailed "word16" "Word16" Nothing
    ]

word32 :: DecodeValue Word32
word32 =
  asum
    [ fromIntegral <$> integer
    , decodeValueFailed "word32" "Word32" Nothing
    ]

word64 :: DecodeValue Word64
word64 =
  asum
    [ fromIntegral <$> integer
    , decodeValueFailed "word64" "Word64" Nothing
    ]

bytestring :: DecodeValue ByteString
bytestring =
  AWS.toBS <$> base64

lazyText :: DecodeValue LT.Text
lazyText =
  LT.fromStrict <$> text

lazyBytestring :: DecodeValue LBS.ByteString
lazyBytestring =
  LBS.fromStrict <$> bytestring

string :: DecodeValue String
string =
  T.unpack <$> text

json :: DecodeValue J.Value
json = do
  t <- text
  case J.eitherDecode (LBS.fromStrict (T.encodeUtf8 t)) of
    Right v -> pure v
    Left e -> decodeValueFailed "json" "Value" (Just (T.pack e))

timeOfDay :: DecodeValue TimeOfDay
timeOfDay = do
  t <- text
  case parseTimeM True defaultTimeLocale "%H:%M:%S%Q" (T.unpack t) of
    Just a -> pure a
    Nothing -> decodeValueFailed "timeOfDay" "TimeOfDay" (Just (T.pack (show t)))

utcTime :: DecodeValue UTCTime
utcTime = do
  t <- text
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t) of
    Just a -> pure a
    Nothing -> decodeValueFailed "utcTime" "UTCTime" Nothing

uuid :: DecodeValue UUID
uuid = do
  t <- text
  case UUID.fromString (T.unpack t) of
    Just a -> pure a
    Nothing -> decodeValueFailed "uuid" "UUID" Nothing

day :: DecodeValue Day
day = do
  t <- text
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
    Just a -> pure a
    Nothing -> decodeValueFailed "day" "Day" Nothing
