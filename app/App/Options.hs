{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Options where

import qualified Amazonka.Data              as AWS
import qualified Data.Text                  as T
import qualified Options.Applicative        as OA

text :: AWS.FromText a => OA.ReadM a
text = OA.eitherReader (AWS.fromText . T.pack)

subParser :: String -> OA.ParserInfo a -> OA.Parser a
subParser availableCommand pInfo =
  OA.hsubparser $ OA.command availableCommand pInfo <> OA.metavar availableCommand

pref :: OA.ParserPrefs
pref =
  OA.prefs $ mconcat
    [ OA.showHelpOnEmpty
    ]
