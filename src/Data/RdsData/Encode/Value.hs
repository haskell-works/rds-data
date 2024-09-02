{-# LANGUAGE BlockArguments #-}

module Data.RdsData.Encode.Value
  ( EncodeValue(..)

  , rdsValue

  , maybe

  , array
  , bool
  , bytestring
  , double
  , integer
  , null
  , text

  , base64
  , day
  , int
  , int8
  , int16
  , int32
  , int64
  , json
  , lazyBytestring
  , lazyText
  , timeOfDay
  , utcTime
  , ulid
  , uuid
  , word
  , word8
  , word16
  , word32
  , word64
  ) where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant
import Data.Int
import Data.RdsData.Encode.Array
import Data.RdsData.Types.Value
import Data.Text (Text)
import Data.Time
import Data.ULID (ULID)
import Data.UUID (UUID)
import Data.Word
import Prelude hiding (maybe, null)

import qualified Amazonka.Bytes                 as AWS
import qualified Amazonka.Data.Base64           as AWS
import qualified Data.Aeson                     as J
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.RdsData.Internal.Convert  as CONV
import qualified Data.Text.Lazy                 as LT
import qualified Prelude                        as P

newtype EncodeValue a = EncodeValue
  { encodeValue :: a -> Value
  }

instance Contravariant EncodeValue where
  contramap f (EncodeValue g) =
    EncodeValue (g . f)

--------------------------------------------------------------------------------

rdsValue :: EncodeValue Value
rdsValue =
  EncodeValue id

--------------------------------------------------------------------------------

maybe :: EncodeValue a -> EncodeValue (Maybe a)
maybe =
  EncodeValue . P.maybe ValueOfNull . encodeValue

--------------------------------------------------------------------------------

array :: EncodeArray a -> EncodeValue a
array enc =
  ValueOfArray . encodeArray enc >$< rdsValue

base64 :: EncodeValue AWS.Base64
base64 =
  ValueOfBase64 >$< rdsValue

bool :: EncodeValue Bool
bool =
  ValueOfBool >$< rdsValue

double :: EncodeValue Double
double =
  ValueOfDouble >$< rdsValue

null :: EncodeValue ()
null =
  const ValueOfNull >$< rdsValue

integer :: EncodeValue Integer
integer =
  ValueOfInteger >$< rdsValue

text :: EncodeValue Text
text =
  ValueOfText >$< rdsValue

--------------------------------------------------------------------------------

int :: EncodeValue Int
int =
  fromIntegral >$< integer

int8 :: EncodeValue Int8
int8 =
  fromIntegral >$< integer

int16 :: EncodeValue Int16
int16 =
  fromIntegral >$< integer

int32 :: EncodeValue Int32
int32 =
  fromIntegral >$< integer

int64 :: EncodeValue Int64
int64 =
  fromIntegral >$< integer

word :: EncodeValue Word
word =
  fromIntegral >$< integer

word8 :: EncodeValue Word8
word8 =
  fromIntegral >$< integer

word16 :: EncodeValue Word16
word16 =
  fromIntegral >$< integer

word32 :: EncodeValue Word32
word32 =
  fromIntegral >$< integer

word64 :: EncodeValue Word64
word64 =
  fromIntegral >$< integer

lazyText :: EncodeValue LT.Text
lazyText =
  LT.toStrict >$< text

bytestring :: EncodeValue ByteString
bytestring =
  (AWS.Base64 . AWS.encodeBase64) >$< base64

lazyBytestring :: EncodeValue LBS.ByteString
lazyBytestring =
  LBS.toStrict >$< bytestring

timeOfDay :: EncodeValue TimeOfDay
timeOfDay =
  CONV.timeOfDayToText >$< text

day :: EncodeValue Day
day =
  CONV.dayToText >$< text

json :: EncodeValue J.Value
json =
  CONV.jsonToText >$< text

utcTime :: EncodeValue UTCTime
utcTime =
  CONV.utcTimeToText >$< text

ulid :: EncodeValue ULID
ulid =
  CONV.ulidToText >$< text

uuid :: EncodeValue UUID
uuid =
  CONV.uuidToText >$< text
