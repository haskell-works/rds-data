{-# LANGUAGE BlockArguments #-}

module Data.RdsData.Encode.Row
  ( EncodeRow(..)

  , rdsValue

  , column

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
  , ulid
  , utcTime
  , uuid
  , word
  , word8
  , word16
  , word32
  , word64
  ) where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Int
import Data.RdsData.Encode.Array (EncodeArray(..))
import Data.RdsData.Encode.Value (EncodeValue(..))
import Data.RdsData.Types.Value
import Data.Text (Text)
import Data.Time
import Data.ULID (ULID)
import Data.UUID (UUID)
import Data.Void
import Data.Word
import Prelude hiding (maybe, null)

import qualified Amazonka.Data.Base64       as AWS
import qualified Data.Aeson                 as J
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.RdsData.Encode.Value  as EV
import qualified Data.Text.Lazy             as LT
import qualified Prelude                    as P

newtype EncodeRow a = EncodeRow
  { encodeRow :: a -> [Value] -> [Value]
  }

instance Contravariant EncodeRow where
  contramap f (EncodeRow g) =
    EncodeRow (g . f)

instance Divisible EncodeRow where
  divide f (EncodeRow g) (EncodeRow h) =
    EncodeRow \a ->
      case f a of
        (b, c) -> g b . h c
  conquer =
    EncodeRow $ const id

instance Decidable EncodeRow where
  choose f (EncodeRow g) (EncodeRow h) =
    EncodeRow \a ->
      case f a of
        Left b -> g b
        Right c -> h c
  lose f =
    EncodeRow $ absurd . f

--------------------------------------------------------------------------------

rdsValue :: EncodeRow Value
rdsValue =
  EncodeRow (:)

--------------------------------------------------------------------------------

column :: EncodeValue a -> EncodeRow a
column (EncodeValue f) =
  EncodeRow \a -> (f a:)

--------------------------------------------------------------------------------

maybe :: EncodeRow a -> EncodeRow (Maybe a)
maybe =
  choose (P.maybe (Left ()) Right) null

--------------------------------------------------------------------------------

array :: EncodeArray a -> EncodeRow a
array =
  column . EV.array

base64 :: EncodeRow AWS.Base64
base64 =
  column EV.base64

bool :: EncodeRow Bool
bool =
  column EV.bool

double :: EncodeRow Double
double =
  column EV.double

null :: EncodeRow ()
null =
  column EV.null

integer :: EncodeRow Integer
integer =
  column EV.integer

text :: EncodeRow Text
text =
  column EV.text

--------------------------------------------------------------------------------

int :: EncodeRow Int
int =
  column EV.int

int8 :: EncodeRow Int8
int8 =
  column EV.int8

int16 :: EncodeRow Int16
int16 =
  column EV.int16

int32 :: EncodeRow Int32
int32 =
  column EV.int32

int64 :: EncodeRow Int64
int64 =
  column EV.int64

word :: EncodeRow Word
word =
  column EV.word

word8 :: EncodeRow Word8
word8 =
  column EV.word8

word16 :: EncodeRow Word16
word16 =
  column EV.word16

word32 :: EncodeRow Word32
word32 =
  column EV.word32

word64 :: EncodeRow Word64
word64 =
  column EV.word64

lazyText :: EncodeRow LT.Text
lazyText =
  column EV.lazyText

bytestring :: EncodeRow ByteString
bytestring =
  column EV.bytestring

lazyBytestring :: EncodeRow LBS.ByteString
lazyBytestring =
  column EV.lazyBytestring

timeOfDay :: EncodeRow TimeOfDay
timeOfDay =
  column EV.timeOfDay

day :: EncodeRow Day
day =
  column EV.day

json :: EncodeRow J.Value
json =
  column EV.json

ulid :: EncodeRow ULID
ulid =
  column EV.ulid

utcTime :: EncodeRow UTCTime
utcTime =
  column EV.utcTime

uuid :: EncodeRow UUID
uuid =
  column EV.uuid
