{-# LANGUAGE BlockArguments #-}

module Data.RdsData.Encode.Array
  ( EncodeArray(..)

  , rdsArray
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
  , timesOfDay
  , ulids
  , uuids
  , utcTimes
  , word16s
  , word32s
  , word64s
  , word8s
  , words
  ) where

import Data.Functor.Contravariant
import Data.Int
import Data.RdsData.Internal.Convert
import Data.RdsData.Types.Array (Array(..))
import Data.Text (Text)
import Data.Time
import Data.ULID
import Data.UUID
import Data.Word
import Prelude hiding (words)

import qualified Data.Aeson           as J
import qualified Data.Text.Lazy       as LT

newtype EncodeArray a = EncodeArray
  { encodeArray :: a -> Array
  }

instance Contravariant EncodeArray where
  contramap f (EncodeArray g) =
    EncodeArray (g . f)

--------------------------------------------------------------------------------

rdsArray :: EncodeArray Array
rdsArray =
  EncodeArray id

--------------------------------------------------------------------------------

arrays :: EncodeArray Array -> EncodeArray [Array]
arrays =
  contramap ArrayOfArrays

--------------------------------------------------------------------------------

bools :: EncodeArray [Bool]
bools =
  ArrayOfBools >$< rdsArray

integers :: EncodeArray [Integer]
integers =
  ArrayOfIntegers >$< rdsArray

texts :: EncodeArray [Text]
texts =
  ArrayOfTexts >$< rdsArray

doubles :: EncodeArray [Double]
doubles =
  ArrayOfDoubles >$< rdsArray

--------------------------------------------------------------------------------

ints :: EncodeArray [Int]
ints =
  fmap fromIntegral >$< integers

int8s :: EncodeArray [Int8]
int8s =
  fmap fromIntegral >$< integers

int16s :: EncodeArray [Int16]
int16s =
  fmap fromIntegral >$< integers

int32s :: EncodeArray [Int32]
int32s =
  fmap fromIntegral >$< integers

int64s :: EncodeArray [Int64]
int64s =
  fmap fromIntegral >$< integers

words :: EncodeArray [Word]
words =
  fmap fromIntegral >$< integers

word8s :: EncodeArray [Word8]
word8s =
  fmap fromIntegral >$< integers

word16s :: EncodeArray [Word16]
word16s =
  fmap fromIntegral >$< integers

word32s :: EncodeArray [Word32]
word32s =
  fmap fromIntegral >$< integers

word64s :: EncodeArray [Word64]
word64s =
  fmap fromIntegral >$< integers

lazyTexts :: EncodeArray [LT.Text]
lazyTexts =
  fmap LT.toStrict >$< texts

timesOfDay :: EncodeArray [TimeOfDay]
timesOfDay =
  fmap timeOfDayToText >$< texts

days :: EncodeArray [Day]
days =
  fmap dayToText >$< texts

jsons :: EncodeArray [J.Value]
jsons =
  fmap jsonToText >$< texts

ulids :: EncodeArray [ULID]
ulids =
  fmap ulidToText >$< texts

uuids :: EncodeArray [UUID]
uuids =
  fmap uuidToText >$< texts

utcTimes :: EncodeArray [UTCTime]
utcTimes =
  fmap utcTimeToText >$< texts
