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
    Constraint(..),
    ForeignKey(..),
  ) where

import           Amazonka.Data             (FromJSON, ToJSON, (.!=), (.:),
                                            (.:?), (.=))
import qualified Amazonka.RDS              as AWS
import qualified Amazonka.SecretsManager   as AWS
import           Control.Applicative
import qualified Data.Aeson                as J
import qualified Data.Aeson.Key            as J
import qualified Data.Aeson.Types          as J
import           Data.Bool
import           Data.Char                 (isAsciiUpper, toLower)
import           Data.Functor
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
    StepOfCreateTable t -> J.toJSON t
    StepOfCreateIndex i -> J.toJSON i

instance FromJSON Step where
  parseJSON v = do
    f <- fieldIn v
      [ "up"
      , "down"
      , "create_table"
      , "create_index"
      ]

    case f of
      "up"           -> StepOfUp          <$> J.parseJSON v
      "down"         -> StepOfDown        <$> J.parseJSON v
      "create_table" -> StepOfCreateTable <$> J.parseJSON v
      "create_index" -> StepOfCreateIndex <$> J.parseJSON v
      _              -> fail "Invalid Step"

fieldIn :: J.Value -> [Text] -> J.Parser Text
fieldIn v fs =
  asum $ fmap (field v) fs

field :: J.Value -> Text -> J.Parser Text
field v f =
  flip (J.withObject "Step") v $ \o ->
    (J..:) @J.Value o (J.fromText f) $> f

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
  { name        :: Text
  , columns     :: [Column]
  , primaryKey  :: Maybe [Text]
  , constraints :: Maybe [Constraint]
  } deriving (Eq, Generic, Show)

instance ToJSON TableSchema where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON TableSchema where
  parseJSON = J.genericParseJSON snakeCaseOptions

data Column = Column
  { name          :: Text
  , type_         :: Text
  , required      :: Bool
  , primaryKey    :: Bool
  , unique        :: Bool
  , autoIncrement :: Bool
  , references    :: Maybe ForeignKey
  } deriving (Eq, Generic, Show)

instance ToJSON Column where
  toJSON v =
    J.object $ catMaybes
      [ "name"            .=? do v ^. the @"name"           & Just
      , "type"            .=? do v ^. the @"type_"          & Just
      , "required"        .=? do v ^. the @"required"       & bool Nothing (Just True)
      , "primary_key"     .=? do v ^. the @"primaryKey"     & bool Nothing (Just True)
      , "unique"          .=? do v ^. the @"unique"         & bool Nothing (Just True)
      , "auto_increment"  .=? do v ^. the @"autoIncrement"  & bool Nothing (Just True)
      , "references"      .=? do v ^. the @"references"     & Just
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
      <*> v .:? "required"        .!= False
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

data ForeignKey = ForeignKey
  { table  :: Text
  , column :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON ForeignKey where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON ForeignKey where
  parseJSON = J.genericParseJSON snakeCaseOptions

data Constraint = Constraint
  { name  :: Text
  , check :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON Constraint where
  toJSON = J.genericToJSON snakeCaseOptions

instance FromJSON Constraint where
  parseJSON = J.genericParseJSON snakeCaseOptions
