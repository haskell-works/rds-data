{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{- HLINT ignore "Monad law, left identity" -}
{- HLINT ignore "Use let" -}

module Data.RdsData.TestTypes
  ( Record(..)
  ) where

import Data.ByteString (ByteString)
import Data.RdsData.Internal.Aeson ()
import Data.Text (Text)
import Data.Time (Day, TimeOfDay(..), UTCTime)
import Data.ULID (ULID)
import Data.UUID (UUID)
import GHC.Generics

import qualified Data.Aeson                   as J

data Record = Record
  { bigint              :: Int
  , bigserial           :: Int
  , boolean             :: Bool
  , bytea               :: ByteString
  , character           :: Text
  , characters          :: Text
  , varyingCharacter    :: Text
  , varyingCharacters   :: Text
  , date                :: Day
  , double              :: Double
  , integer             :: Integer
  , json                :: J.Value
  , jsonb               :: J.Value
  , numeric             :: Text
  , numerics            :: Text
  , real                :: Double
  , smallint            :: Int
  , smallserial         :: Int
  , serial              :: Int
  , text                :: Text
  , time                :: TimeOfDay
  , times               :: TimeOfDay
  , timestamp           :: UTCTime
  , timestamps          :: UTCTime
  , ulid                :: ULID
  , uuid                :: UUID
  }
  deriving (Show, Eq, Generic)
