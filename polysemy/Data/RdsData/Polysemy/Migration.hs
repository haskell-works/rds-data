{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- HLINT ignore "Use let" -}

module Data.RdsData.Polysemy.Migration
  ( migrateDown,
    migrateUp,
  ) where

import qualified Amazonka.Env                   as AWS
import qualified Amazonka.Types                 as AWS
import qualified Data.Aeson                     as J
import qualified Data.ByteString.Lazy           as LBS
import           Data.Generics.Product.Any
import qualified Data.List                      as L
import           Data.RdsData.Aws
import           Data.RdsData.Migration.Types   hiding (id)
import           Data.RdsData.Polysemy.Core
import           Data.RdsData.Polysemy.Error
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.File
import           HaskellWorks.Polysemy.Prelude
import           Lens.Micro

migrateDown :: ()
  => Member (DataLog AwsLogEntry) r
  => Member (Embed IO) r
  => Member (Error AWS.Error) r
  => Member (Error IOException) r
  => Member (Error JsonDecodeError) r
  => Member (Error RdsDataError) r
  => Member (Error YamlDecodeError) r
  => Member (Reader AWS.Env) r
  => Member (Reader StatementContext) r
  => Member Log r
  => Member Resource r
  => FilePath
  -> Sem r ()
migrateDown migrationFp = do
  value :: Migration <- readYamlFile migrationFp

  let theSteps = value ^.. the @"plan" . to reverse . each . the @"steps" . _Just . to reverse . each

  forM_ theSteps $ \case
      StepOfDown downStep -> do
        info $ "Executing statement: " <> tshow downStep

        let statement = downStep ^. the @"down" . the @1

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))
      StepOfUp _ -> pure ()
      StepOfCreateTable createTableStatement -> do
        statement <- pure $ mconcat
          [ "DROP TABLE " <> createTableStatement ^. the @"createTable" . the @"name"
          ]

        info $ "Executing statement: " <> statement

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))
      StepOfCreateIndex createIndexStatement -> do
        statement <- pure $ mconcat
          [ "DROP INDEX " <> createIndexStatement ^. the @"createIndex" . the @"name"
          ]

        info $ "Executing statement: " <> statement

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))

migrateUp :: ()
  => Member (DataLog AwsLogEntry) r
  => Member (Embed IO) r
  => Member (Error AWS.Error) r
  => Member (Error IOException) r
  => Member (Error JsonDecodeError) r
  => Member (Error RdsDataError) r
  => Member (Error YamlDecodeError) r
  => Member (Reader AWS.Env) r
  => Member (Reader StatementContext) r
  => Member Log r
  => Member Resource r
  => FilePath
  -> Sem r ()
migrateUp migrationFp = do
  value :: Migration <- readYamlFile migrationFp

  let theSteps = value ^.. the @"plan" . each . the @"steps"  . _Just . each

  forM_ theSteps $ \case
      StepOfUp upStep -> do
        info $ "Executing statement: " <> tshow upStep

        let statement = upStep ^. the @"up" . the @1

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))
      StepOfDown _ -> pure ()
      StepOfCreateTable createTableStatement -> do
        columnClauses <- pure $
          createTableStatement ^.. the @"createTable" . the @"columns" . each . to columnToText

        constraintClauses <- pure $
          createTableStatement ^.. the @"createTable" . the @"constraints" . _Just . each . to constraintToText

        statement <- pure $ mconcat
          [ "CREATE TABLE " <> createTableStatement ^. the @"createTable" . the @"name" <> " ("
          , mconcat $ L.intersperse ", " (columnClauses <> constraintClauses)
          , ");\n"
          ]

        info $ "Executing create table statement: " <> statement

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))
      StepOfCreateIndex createIndexStatement -> do
        columnClauses <- pure $
          createIndexStatement ^.. the @"createIndex" . the @"columns" . each

        statement <- pure $ mconcat
          [ "CREATE INDEX " <> createIndexStatement ^. the @"createIndex" . the @"name"
          , " ON " <> createIndexStatement ^. the @"createIndex" . the @"table" <> " ("
          , mconcat $ L.intersperse ", " columnClauses
          , ");\n"
          ]

        info $ "Executing  create index statement: " <> statement

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))

columnToText :: Column -> Text
columnToText c =
  T.intercalate " " $ concat
    [ [c ^. the @"name"]
    , [c ^. the @"type_"]
    , [ "NOT NULL"
      | c ^. the @"required"
      ]
    , [ "PRIMARY KEY"
      | c ^. the @"primaryKey"
      ]
    , [ "UNIQUE"
      | c ^. the @"unique"
      ]
    , [ "AUTO_INCREMENT"
      | c ^. the @"autoIncrement"
      ]
    , [ [ "REFERENCES"
        , fk ^. the @"table"
        , "("
        , fk ^. the @"column"
        , ")"
        ] & T.intercalate " "
      | Just fk <- [c ^. the @"references"]
      ]
    ]

constraintToText :: Constraint -> Text
constraintToText c =
  T.intercalate " "
    [ "CONSTRAINT"
    , c ^. the @"name"
    , "CHECK"
    , "("
    , c ^. the @"check"
    , ")"
    ]
