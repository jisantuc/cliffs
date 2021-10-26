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
import Data.Text (pack)
import qualified Graphics.Vty as V
import Lib
  ( AppState (..),
    Script (Script),
    appEvent,
    drawUi,
    emphAttr,
  )
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
  scriptFileNames <- listDirectory "./scripts"
  let scripts =
        AppState
          ((\n -> Script False (pack $ takeFileName n) "description goes here") <$> scriptFileNames)
          Nothing
   in void $ M.defaultMain app scripts
