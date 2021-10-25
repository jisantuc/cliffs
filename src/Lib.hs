{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Brick (AttrName, BrickEvent (VtyEvent), Widget, padLeftRight, str, txt, withAttr, withBorderStyle)
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.Border.Style
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Table (Table, renderTable, rowBorders, surroundingBorder, table)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Text (Text)
import qualified Graphics.Vty as V

data Script = Script {selected :: Bool, name :: Text, description :: Text} deriving (Eq, Show)

data Scripts = Scripts {availableScripts :: [Script], currentScript :: Maybe Script} deriving (Eq, Show)

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

selectNext :: Scripts -> Scripts
selectNext (Scripts [] _) = Scripts [] Nothing
selectNext (Scripts xs@(h : t) _) =
  if not $ any selected xs
    then
      let selectedScript = h {selected = True}
       in Scripts (selectedScript : t) (Just selectedScript)
    else
      let (selectedScript, scripts) = selectNext' (h :| t)
       in Scripts (toList scripts) (Just selectedScript)

selectPrevious :: Scripts -> Scripts
selectPrevious (Scripts xs s) =
  let Scripts newElems selected = selectNext (Scripts (reverse xs) s)
   in Scripts (reverse newElems) selected

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

drawTable :: Scripts -> Table a
drawTable (Scripts {availableScripts}) =
  table $ mkRow <$> availableScripts

drawUi :: Scripts -> [Widget ()]
drawUi scripts =
  let t =
        surroundingBorder False $
          rowBorders False $
            drawTable scripts
      attrs =
        C.hCenter $ borderWithLabel (str "Scripts") . renderTable $ t
      ui =
        C.vCenter $
          withBorderStyle Brick.Widgets.Border.Style.unicodeBold attrs
   in [ui]

appEvent :: Scripts -> BrickEvent () e -> T.EventM () (T.Next Scripts)
appEvent i (VtyEvent (V.EvKey V.KEsc [])) = M.halt i
appEvent i (VtyEvent (V.EvKey V.KDown [])) = M.continue (selectNext i)
appEvent i (VtyEvent (V.EvKey V.KUp [])) = M.continue (selectPrevious i)
appEvent i (VtyEvent (V.EvKey V.KEnter [])) = M.continue i
appEvent i _ = M.continue i

emphAttr :: AttrName
emphAttr = "emphasis"
