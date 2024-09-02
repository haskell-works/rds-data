{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.RdsData.Decode.Row
  ( DecodeRow(..)
  , integer
  , int
  , int8
  , int16
  , int32
  , int64
  , bool
  , double
  , string
  , text
  , lazyText
  , word
  , word8
  , word16
  , word32
  , word64
  , bytestring
  , lazyBytestring
  , timeOfDay
  , day
  , ulid
  , utcTime
  , uuid
  , ignore
  , json
  , maybe
  , column
  , decodeRow
  , decodeRows
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString (ByteString)
import Control.Monad
import Data.Functor.Identity ( Identity )
import Data.Int
import Data.RdsData.Decode.Value (DecodeValue)
import Data.RdsData.Types.Value
import Data.Text
import Data.Time
import Data.ULID (ULID)
import Data.UUID (UUID)
import Data.Word
import Prelude hiding (maybe)

import qualified Data.Aeson                    as J
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.RdsData.Decode.Value     as DV
import qualified Data.RdsData.Internal.Convert as CONV
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.UUID                     as UUID

newtype DecodeRow a = DecodeRow
  { unDecodeRow :: ExceptT Text (StateT [Value] Identity) a
  }
  deriving (Applicative, Functor, Monad, MonadState [Value], MonadError Text)

instance MonadFail DecodeRow where
  fail = DecodeRow . throwError . pack

maybe :: DecodeRow a -> DecodeRow (Maybe a)
maybe r = do
  cs <- get
  case cs of
    ValueOfNull : vs -> do
      put vs
      pure Nothing
    _ -> Just <$> r

decodeRowValue :: ()
  => MonadError Text m
  => DecodeValue a
  -> Value
  -> m a
decodeRowValue decoder v =
  case DV.decodeValue decoder v of
    Right a -> pure a
    Left e -> throwError $ "Failed to decode Value: " <> e

column :: ()
  => DecodeValue a
  -> DecodeRow a
column decoder = do
  cs <- get
  case cs of
    v : vs -> do
      s <- decodeRowValue decoder v
      put vs
      pure s
    [] -> do
      throwError "Expected RdsText, but got no more values in row."

integer :: DecodeRow Integer
integer =
  column DV.integer

int :: DecodeRow Int
int =
  column DV.int

int8 :: DecodeRow Int8
int8 =
  column DV.int8

int16 :: DecodeRow Int16
int16 =
  column DV.int16

int32 :: DecodeRow Int32
int32 =
  column DV.int32

int64 :: DecodeRow Int64
int64 =
  column DV.int64

word :: DecodeRow Word
word =
  column DV.word

word8 :: DecodeRow Word8
word8 =
  column DV.word8

word16 :: DecodeRow Word16
word16 =
  column DV.word16

word32 :: DecodeRow Word32
word32 =
  column DV.word32

word64 :: DecodeRow Word64
word64 =
  column DV.word64

text :: DecodeRow Text
text =
  column DV.text

lazyText :: DecodeRow LT.Text
lazyText =
  column DV.lazyText

bool :: DecodeRow Bool
bool =
  column DV.bool

double :: DecodeRow Double
double =
  column DV.double

bytestring :: DecodeRow ByteString
bytestring =
  column DV.bytestring

lazyBytestring :: DecodeRow LBS.ByteString
lazyBytestring =
  column DV.lazyBytestring

string :: DecodeRow String
string =
  column DV.string

json :: DecodeRow J.Value
json =
  column DV.json

timeOfDay :: DecodeRow TimeOfDay
timeOfDay = do
  t <- text
  case parseTimeM True defaultTimeLocale "%H:%M:%S%Q" (T.unpack t) of
    Just a -> pure a
    Nothing -> throwError $ "Failed to parse TimeOfDay: " <> T.pack (show t)

ulid :: DecodeRow ULID
ulid = do
  t <- text
  case CONV.textToUlid t of
    Right a -> pure a
    Left msg -> throwError $ "Failed to parse ULID: " <> msg

utcTime :: DecodeRow UTCTime
utcTime = do
  t <- text
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t) of
    Just a -> pure a
    Nothing -> throwError $ "Failed to parse UTCTime: " <> T.pack (show t)

uuid :: DecodeRow UUID
uuid = do
  t <- text
  case UUID.fromString (T.unpack t) of
    Just a -> pure a
    Nothing -> throwError $ "Failed to parse UUID: " <> T.pack (show t)

day :: DecodeRow Day
day = do
  t <- text
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
    Just a -> pure a
    Nothing -> throwError $ "Failed to parse Day: " <> T.pack (show t)

ignore :: DecodeRow ()
ignore =
  void $ column DV.rdsValue

decodeRow :: DecodeRow a -> [Value] -> Either Text a
decodeRow r = evalState (runExceptT (unDecodeRow r))

decodeRows :: DecodeRow a ->  [[Value]] -> Either Text [a]
decodeRows r = traverse (decodeRow r)
