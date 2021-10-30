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
import Data.Text (pack)
import qualified Graphics.Vty as V
import Lib
  ( AppState (..),
    Script (Script),
    appEvent,
    drawUi,
    emphAttr,
  )
import ScriptMetadata (findScriptDescriptions)
import System.Directory (listDirectory)
import System.FilePath (takeFileName)

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

main :: IO ()
main = do
  descMap <- findScriptDescriptions
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
