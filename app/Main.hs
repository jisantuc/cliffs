{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick (on)
import Brick.AttrMap
  ( attrMap,
  )
import qualified Brick.Main as M
import Control.Monad (void)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import qualified Graphics.Vty as V
import Lib
  ( AppState (..),
    Script (Script),
    appEvent,
    drawUi,
    emphAttr,
  )
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    header,
    helper,
    info,
    long,
    progDesc,
    short,
    showDefault,
    strOption,
    value,
    (<**>),
  )
import ScriptMetadata (findScriptDescriptions)
import System.Directory (listDirectory)
import System.FilePath (takeFileName)

data Options = Options
  { nameColumnName :: Text,
    descriptionColumnName :: Text
  }

options :: Parser Options
options =
  Options
    <$> strOption (long "script-name-column" <> short 's' <> showDefault <> value "Script Name")
    <*> strOption (long "description-column" <> short 'd' <> showDefault <> value "Description")

app :: M.App AppState (IO ()) ()
app =
  M.App
    { M.appDraw = drawUi,
      M.appStartEvent = return,
      M.appHandleEvent = appEvent,
      -- A function from an app state to a map from attributes (string classses basically)
      -- to modifications to rendering
      M.appAttrMap = const $ attrMap V.defAttr [(emphAttr, V.white `on` V.blue)],
      M.appChooseCursor = M.neverShowCursor
    }

run :: Options -> IO ()
run (Options {nameColumnName, descriptionColumnName}) = do
  descMap <- findScriptDescriptions nameColumnName descriptionColumnName
  scriptFileNames <- listDirectory "./scripts"
  let lookupDesc filePath =
        foldr const "No description" $ descMap >>= \m -> M.lookup (pack $ takeFileName filePath) m
      scripts =
        AppState
          ( ( \n ->
                Script False (pack $ takeFileName n) (lookupDesc n)
            )
              <$> scriptFileNames
          )
          Nothing
   in void $ M.defaultMain app scripts

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Launch an interactive terminal UI for inspecting scripts-to-rule-them-all"
            <> header "cliffs"
        )
