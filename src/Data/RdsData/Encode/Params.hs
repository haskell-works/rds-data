{-# LANGUAGE BlockArguments #-}

module Data.RdsData.Encode.Params
  ( EncodeParams(..)

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
import Data.RdsData.Encode.Param (EncodeParam(..))
import Data.RdsData.Types.Param
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import Data.ULID (ULID)
import Data.Void
import Data.Word
import Prelude hiding (maybe, null)

import qualified Amazonka.Data.Base64       as AWS
import qualified Data.Aeson                 as J
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.RdsData.Encode.Param  as EP
import qualified Data.Text.Lazy             as LT
import qualified Prelude                    as P

newtype EncodeParams a = EncodeParams
  { encodeParams :: a -> [Param] -> [Param]
  }

instance Contravariant EncodeParams where
  contramap f (EncodeParams g) =
    EncodeParams (g . f)

instance Divisible EncodeParams where
  divide f (EncodeParams g) (EncodeParams h) =
    EncodeParams \a ->
      case f a of
        (b, c) -> g b . h c
  conquer =
    EncodeParams $ const id

instance Decidable EncodeParams where
  choose f (EncodeParams g) (EncodeParams h) =
    EncodeParams \a ->
      case f a of
        Left b -> g b
        Right c -> h c
  lose f =
    EncodeParams $ absurd . f

--------------------------------------------------------------------------------

rdsValue :: EncodeParams Param
rdsValue =
  EncodeParams (:)

--------------------------------------------------------------------------------

column :: EncodeParam a -> EncodeParams a
column (EncodeParam f) =
  EncodeParams \a -> (f a:)

--------------------------------------------------------------------------------

maybe :: EncodeParams a -> EncodeParams (Maybe a)
maybe =
  choose (P.maybe (Left ()) Right) null

--------------------------------------------------------------------------------

array :: EncodeArray a -> EncodeParams a
array =
  column . EP.array

base64 :: EncodeParams AWS.Base64
base64 =
  column EP.base64

bool :: EncodeParams Bool
bool =
  column EP.bool

double :: EncodeParams Double
double =
  column EP.double

null :: EncodeParams ()
null =
  column EP.null

integer :: EncodeParams Integer
integer =
  column EP.integer

text :: EncodeParams Text
text =
  column EP.text

--------------------------------------------------------------------------------

int :: EncodeParams Int
int =
  column EP.int

int8 :: EncodeParams Int8
int8 =
  column EP.int8

int16 :: EncodeParams Int16
int16 =
  column EP.int16

int32 :: EncodeParams Int32
int32 =
  column EP.int32

int64 :: EncodeParams Int64
int64 =
  column EP.int64

word :: EncodeParams Word
word =
  column EP.word

word8 :: EncodeParams Word8
word8 =
  column EP.word8

word16 :: EncodeParams Word16
word16 =
  column EP.word16

word32 :: EncodeParams Word32
word32 =
  column EP.word32

word64 :: EncodeParams Word64
word64 =
  column EP.word64

lazyText :: EncodeParams LT.Text
lazyText =
  column EP.lazyText

bytestring :: EncodeParams ByteString
bytestring =
  column EP.bytestring

lazyBytestring :: EncodeParams LBS.ByteString
lazyBytestring =
  column EP.lazyBytestring

timeOfDay :: EncodeParams TimeOfDay
timeOfDay =
  column EP.timeOfDay

day :: EncodeParams Day
day =
  column EP.day

json :: EncodeParams J.Value
json =
  column EP.json

ulid :: EncodeParams ULID
ulid =
  column EP.ulid

utcTime :: EncodeParams UTCTime
utcTime =
  column EP.utcTime

uuid :: EncodeParams UUID
uuid =
  column EP.uuid
