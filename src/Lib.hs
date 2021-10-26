{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Brick (AttrName, BrickEvent (VtyEvent), Widget, hLimit, padLeftRight, str, strWrap, txt, vBox, withAttr, withBorderStyle)
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.Border.Style
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Table (Table, renderTable, rowBorders, surroundingBorder, table)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Graphics.Vty as V
import System.Command (Stdout (Stdout), command)

data Script = Script
  { selected :: Bool,
    name :: Text,
    description :: Text
  }
  deriving (Eq, Show)

data ScriptWithOutput = ScriptWithOutput
  { script :: Script,
    output :: Maybe String
  }
  deriving (Eq, Show)

data AppState = AppState
  { availableAppState :: [Script],
    currentScript :: Maybe ScriptWithOutput
  }
  deriving (Eq, Show)

scriptWithoutOutput :: Script -> ScriptWithOutput
scriptWithoutOutput s = ScriptWithOutput s Nothing

selectNext' :: NonEmpty Script -> (Script, NonEmpty Script)
selectNext' (h :| []) =
  let selectedScript = h {selected = True}
   in (selectedScript, selectedScript :| [])
selectNext' (h :| t : ts) =
  if (selected h)
    then
      let selectedScript = t {selected = True}
       in (t, h {selected = False} :| selectedScript : ts)
    else
      let (selectedScript, scripts) = selectNext' (t :| ts)
       in (selectedScript, h :| toList scripts)

selectNext :: AppState -> AppState
selectNext (AppState [] _) = AppState [] Nothing
selectNext (AppState xs@(h : t) _) =
  if not $ any selected xs
    then
      let selectedScript = h {selected = True}
       in AppState (selectedScript : t) (Just . scriptWithoutOutput $ selectedScript)
    else
      let (selectedScript, scripts) = selectNext' (h :| t)
       in AppState (toList scripts) (Just . scriptWithoutOutput $ selectedScript)

selectPrevious :: AppState -> AppState
selectPrevious (AppState xs s) =
  let AppState newElems selected = selectNext (AppState (reverse xs) s)
   in AppState (reverse newElems) selected

listDrawName :: Bool -> Text -> Widget a
listDrawName selected name =
  let selStr s =
        if selected
          then withAttr emphAttr $ txt s
          else txt s
   in selStr name

listDrawDescription :: Text -> Widget a
listDrawDescription description =
  padLeftRight 1 $ txt description

mkRow :: Script -> [Widget a]
mkRow (Script {selected, name, description}) =
  [ padLeftRight 3 $ listDrawName selected name,
    listDrawDescription description
  ]

drawTable :: AppState -> Table a
drawTable (AppState {availableAppState}) =
  table $ mkRow <$> availableAppState

drawUi :: AppState -> [Widget ()]
drawUi appState =
  let t =
        surroundingBorder False $
          rowBorders False $
            drawTable appState
      attrs =
        C.hCenter $ borderWithLabel (str "Scripts") . renderTable $ t
      helpBox =
        let helpText = fromMaybe "No help text yet" (currentScript appState >>= output)
         in C.hCenter . hLimit 90 . borderWithLabel (str "Help output") . strWrap $ helpText
      ui =
        vBox
          [ C.vCenter $
              withBorderStyle Brick.Widgets.Border.Style.unicodeBold attrs,
            C.vCenter helpBox
          ]
   in [ui]

getHelp :: Script -> IO Stdout
getHelp Script {name} = command [] ("./scripts/" ++ unpack name) ["--help"]

addHelpOutput :: (AppState -> AppState) -> AppState -> T.EventM e (T.Next AppState)
addHelpOutput successorFunc state =
  let nextScript = successorFunc state
      nextSelected = currentScript nextScript
   in ( liftIO $ do
          helpOut <- traverse getHelp (script <$> nextSelected)
          case helpOut of
            Nothing -> pure nextScript
            Just (Stdout helpText) -> pure nextScript {currentScript = (\x -> x {output = Just helpText}) <$> nextSelected}
      )
        >>= M.continue

appEvent :: AppState -> BrickEvent () (IO ()) -> T.EventM () (T.Next AppState)
appEvent i (VtyEvent (V.EvKey V.KEsc [])) = M.halt i
appEvent i (VtyEvent (V.EvKey V.KDown [])) = addHelpOutput selectNext i
appEvent i (VtyEvent (V.EvKey V.KUp [])) = addHelpOutput selectPrevious i
appEvent i (VtyEvent (V.EvKey V.KEnter [])) = M.continue i
appEvent i _ = M.continue i

emphAttr :: AttrName
emphAttr = "emphasis"
