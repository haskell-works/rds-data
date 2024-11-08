{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Redundant flip" -}

module Data.RdsData.Encode.Param
  ( EncodeParam(..)

  , rdsParam

  , named
  , typed

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
  , int16
  , int32
  , int64
  , int8
  , json
  , lazyBytestring
  , base64Text
  , lazyBase64Text
  , lazyText
  , timeOfDay
  , ulid
  , utcTime
  , uuid
  , word
  , word16
  , word32
  , word64
  , word8
  ) where

import           Data.ByteString               (ByteString)
import           Data.Functor.Contravariant
import           Data.Generics.Product.Any
import           Data.Int
import           Data.RdsData.Encode.Array
import           Data.RdsData.Types.Param
import           Data.RdsData.Types.Value
import           Data.Text                     (Text)
import           Data.Time
import           Data.ULID                     (ULID)
import           Data.UUID                     (UUID)
import           Data.Word
import           Lens.Micro
import           Prelude                       hiding (maybe, null)

import qualified Amazonka.Bytes                as AWS
import qualified Amazonka.Data.Base64          as AWS
import qualified Amazonka.RDSData              as AWS
import qualified Data.Aeson                    as J
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Base64.Lazy   as LB64
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.RdsData.Internal.Convert as CONV
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT
import qualified Prelude                       as P

newtype EncodeParam a = EncodeParam
  { encodeParam :: a -> Param
  }

instance Contravariant EncodeParam where
  contramap f (EncodeParam g) =
    EncodeParam (g . f)

--------------------------------------------------------------------------------

named :: Text -> EncodeParam a -> EncodeParam a
named n (EncodeParam f) =
  EncodeParam \a -> f a & the @"name" .~ Just n

typed :: AWS.TypeHint -> EncodeParam a -> EncodeParam a
typed t (EncodeParam f) =
  EncodeParam \a -> f a & the @"hint" .~ Just t

rdsParam :: EncodeParam Param
rdsParam =
  EncodeParam id

--------------------------------------------------------------------------------

maybe :: EncodeParam a -> EncodeParam (Maybe a)
maybe =
  EncodeParam . P.maybe (Param Nothing Nothing ValueOfNull) . (.encodeParam)

--------------------------------------------------------------------------------

array :: EncodeArray a -> EncodeParam a
array enc =
  Param Nothing Nothing . ValueOfArray . enc.encodeArray >$< rdsParam

base64 :: EncodeParam AWS.Base64
base64 =
  Param Nothing Nothing . ValueOfBase64 >$< rdsParam

bool :: EncodeParam Bool
bool =
  Param Nothing Nothing . ValueOfBool >$< rdsParam

double :: EncodeParam Double
double =
  Param Nothing Nothing . ValueOfDouble >$< rdsParam

null :: EncodeParam ()
null =
  Param Nothing Nothing . const ValueOfNull >$< rdsParam

integer :: EncodeParam Integer
integer =
  Param Nothing Nothing . ValueOfInteger >$< rdsParam

text :: EncodeParam Text
text =
  Param Nothing Nothing . ValueOfText >$< rdsParam

--------------------------------------------------------------------------------

int :: EncodeParam Int
int =
  fromIntegral >$< integer

int8 :: EncodeParam Int8
int8 =
  fromIntegral >$< integer

int16 :: EncodeParam Int16
int16 =
  fromIntegral >$< integer

int32 :: EncodeParam Int32
int32 =
  fromIntegral >$< integer

int64 :: EncodeParam Int64
int64 =
  fromIntegral >$< integer

word :: EncodeParam Word
word =
  fromIntegral >$< integer

word8 :: EncodeParam Word8
word8 =
  fromIntegral >$< integer

word16 :: EncodeParam Word16
word16 =
  fromIntegral >$< integer

word32 :: EncodeParam Word32
word32 =
  fromIntegral >$< integer

word64 :: EncodeParam Word64
word64 =
  fromIntegral >$< integer

lazyText :: EncodeParam LT.Text
lazyText =
  LT.toStrict >$< text

bytestring :: EncodeParam ByteString
bytestring =
  (AWS.Base64 . AWS.encodeBase64) >$< base64

lazyBytestring :: EncodeParam LBS.ByteString
lazyBytestring =
  LBS.toStrict >$< bytestring

base64Text :: EncodeParam ByteString
base64Text =
  (T.decodeUtf8 . B64.encode) >$< text

lazyBase64Text :: EncodeParam LBS.ByteString
lazyBase64Text =
  (LT.decodeUtf8 . LB64.encode) >$< lazyText

timeOfDay :: EncodeParam TimeOfDay
timeOfDay =
  CONV.timeOfDayToText >$< text & typed AWS.TypeHint_TIME

day :: EncodeParam Day
day =
  CONV.dayToText >$< text & typed AWS.TypeHint_DATE

json :: EncodeParam J.Value
json =
  CONV.jsonToText >$< text & typed AWS.TypeHint_JSON

ulid :: EncodeParam ULID
ulid =
  CONV.ulidToText >$< text

utcTime :: EncodeParam UTCTime
utcTime =
  CONV.utcTimeToText >$< text & typed AWS.TypeHint_TIMESTAMP

uuid :: EncodeParam UUID
uuid =
  CONV.uuidToText >$< text & typed AWS.TypeHint_UUID
