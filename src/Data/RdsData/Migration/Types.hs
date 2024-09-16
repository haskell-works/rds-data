{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module Data.RdsData.Migration.Types
  ( MigrationRow(..),
    RdsClusterDetails(..),
    Migration(..),
    Delta(..),
    Step(..),
    Statement(..),
    CreateTableStep(..),
    TableSchema(..),
    Column(..),
    ForeignKey(..),
  ) where

import           Amazonka.Data             (FromJSON, ToJSON, (.!=), (.:),
                                            (.:?), (.=))
import qualified Amazonka.RDS              as AWS
import qualified Amazonka.SecretsManager   as AWS
import           Control.Applicative
import qualified Data.Aeson                as J
import           Data.Bool
import           Data.Char                 (isAsciiUpper, toLower)
import           Data.Generics.Product.Any
import           Data.Maybe
import           Data.RdsData.Orphans      ()
import           Data.Text                 (Text)
import           Data.Time
import           Data.ULID                 (ULID)
import           GHC.Generics
import           Lens.Micro

-- Helper to transform field names to snake_case
snakeCaseOptions :: J.Options
snakeCaseOptions = J.defaultOptions { J.fieldLabelModifier = camelToSnake }

-- Helper function to convert camelCase to snake_case
camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x:xs) = toLower x : go xs
  where
    go [] = []
    go "_" = []
    go (y:ys)
      | isAsciiUpper y = '_' : toLower y : go ys
      | otherwise      = y : go ys

data MigrationRow = MigrationRow
  { uuid       :: ULID
  , createdBy  :: UTCTime
  , deployedBy :: Text
  } deriving (Eq, Show, Generic)

data RdsClusterDetails = RdsClusterDetails
  { createDbClusterResponse :: AWS.CreateDBClusterResponse
  , createSecretResponse    :: AWS.CreateSecretResponse
  } deriving (Eq, Generic, Show)

data Migration = Migration
  { description :: Text
  , plan        :: [Delta]
  } deriving (Eq, Generic, Show)

instance ToJSON Migration where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON Migration where
  parseJSON = J.genericParseJSON snakeCaseOptions

data Delta = Delta
  { id          :: ULID
  , description :: Text
  , steps       :: Maybe [Step]
  } deriving (Eq, Generic, Show)

instance ToJSON Delta where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON Delta where
  parseJSON = J.genericParseJSON snakeCaseOptions

data Step =
    StepOfUp UpStep
  | StepOfDown DownStep
  | StepOfCreateTable CreateTableStep
  | StepOfCreateIndex CreateIndexStep
  deriving (Eq, Show)

instance ToJSON Step where
  toJSON = \case
    StepOfUp step -> J.toJSON step
    StepOfDown step -> J.toJSON step
    StepOfCreateTable table -> J.toJSON table
    StepOfCreateIndex index -> J.toJSON index

instance FromJSON Step where
  parseJSON v =
    flip (J.withObject "Step") v $ \_ ->
      asum
        [ StepOfUp          <$> J.parseJSON v
        , StepOfDown        <$> J.parseJSON v
        , StepOfCreateTable <$> J.parseJSON v
        , StepOfCreateIndex <$> J.parseJSON v
        ]

newtype UpStep = UpStep
  { up   :: Statement
  } deriving (Eq, Generic, Show)

instance ToJSON UpStep where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON UpStep where
  parseJSON = J.genericParseJSON snakeCaseOptions

newtype DownStep = DownStep
  { down :: Statement
  } deriving (Eq, Generic, Show)

instance ToJSON DownStep where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON DownStep where
  parseJSON = J.genericParseJSON snakeCaseOptions

newtype Statement = Statement Text
  deriving (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

newtype CreateTableStep = CreateTableStep
  { createTable :: TableSchema
  } deriving (Eq, Generic, Show)

instance ToJSON CreateTableStep where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON CreateTableStep where
  parseJSON = J.genericParseJSON snakeCaseOptions

newtype CreateIndexStep = CreateIndexStep
  { createIndex :: IndexSchema
  } deriving (Eq, Generic, Show)

instance ToJSON CreateIndexStep where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON CreateIndexStep where
  parseJSON = J.genericParseJSON snakeCaseOptions

data TableSchema = TableSchema
  { name    :: Text
  , columns :: [Column]
  } deriving (Eq, Generic, Show)

instance ToJSON TableSchema where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON TableSchema where
  parseJSON = J.genericParseJSON snakeCaseOptions

data Column = Column
  { name          :: Text
  , type_         :: Text
  , nullable      :: Bool
  , primaryKey    :: Bool
  , unique        :: Bool
  , autoIncrement :: Bool
  , references    :: Maybe ForeignKey
  } deriving (Eq, Generic, Show)

instance ToJSON Column where
  toJSON column =
    J.object $ catMaybes
      [ "name"            .=? do column ^. the @"name"          & Just
      , "type"            .=? do column ^. the @"type_"         & Just
      , "nullable"        .=? do column ^. the @"nullable"      & bool Nothing (Just True)
      , "primary_key"     .=? do column ^. the @"primaryKey"    & bool Nothing (Just True)
      , "unique"          .=? do column ^. the @"unique"        & bool Nothing (Just True)
      , "auto_increment"  .=? do column ^. the @"autoIncrement" & bool Nothing (Just True)
      , "references"      .=? do column ^. the @"references"    & Just
      ]

(.=?) :: (J.KeyValue e kv, ToJSON v) => J.Key -> Maybe v -> Maybe kv
(.=?) k mv =
  case mv of
    Just v  -> Just $ k .= v
    Nothing -> Nothing

instance FromJSON Column where
  parseJSON = J.withObject "Column" $ \v ->
    Column
      <$> v .:  "name"
      <*> v .:  "type"
      <*> v .:? "nullable"        .!= False
      <*> v .:? "primary_key"     .!= False
      <*> v .:? "unique"          .!= False
      <*> v .:? "auto_increment"  .!= False
      <*> v .:? "references"

data IndexSchema = IndexSchema
  { name    :: Text
  , table   :: Text
  , columns :: [Text]
  } deriving (Eq, Generic, Show)

instance ToJSON IndexSchema where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON IndexSchema where
  parseJSON = J.genericParseJSON snakeCaseOptions


newtype ForeignKey = ForeignKey Text
  deriving newtype (Eq, Show, ToJSON, FromJSON)
  deriving Generic
