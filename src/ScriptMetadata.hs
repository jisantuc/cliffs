{-# LANGUAGE OverloadedStrings #-}

module ScriptMetadata (findScriptDescriptions, readScriptDescriptions, getContainedText) where

import CMarkGFM (Node (Node), NodeType (CODE, LINK, TABLE, TABLE_ROW, TEXT), commonmarkToNode, extTable)
-- import qualified Data.Map.Strict as M
-- import Data.Text (Text)

import qualified Data.Map.Lazy as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as T (readFile)

findScriptDescriptions :: Text -> Text -> IO (Maybe (M.Map Text Text))
findScriptDescriptions
  scriptNameColumnName
  scriptDescriptionColumnName =
    readScriptDescriptions scriptNameColumnName scriptDescriptionColumnName . commonmarkToNode [] [extTable]
      <$> T.readFile "README.md"

rowToPair :: Node -> Maybe (M.Map Text Text)
rowToPair (Node _ TABLE_ROW (c1 : c2 : _)) =
  M.singleton <$> getContainedText c1 <*> getContainedText c2
rowToPair _ = Nothing

-- keys should be extracted from:
-- plain text script names (e.g. "update")
-- code script names (e.g. "`update`")
-- code or plain text script names that links to the script (e.g. "[`update`](./scripts/update)")
--
-- descriptions should always be treated as plain text
extractMap :: Node -> Maybe (M.Map Text Text)
extractMap (Node _ (TABLE _) (_ : rows)) =
  -- for each row, grab the first and second cells.
  -- treat the first as the script name and second as the description
  -- ignore a few kinds of failure:
  --   - if there isn't a second cell (possible! even parses correctly!)
  --   - if we can't get text out for some reason
  -- at this stage the table is assumed to be well-formed and cooperative with
  -- our goals
  mconcat $ rowToPair <$> rows
extractMap _ = Nothing

joinMaybeTexts :: [Maybe Text] -> Text
joinMaybeTexts [] = mempty
joinMaybeTexts extractions =
  T.unwords $
    foldr
      ( \x acc -> case x of
          Nothing -> acc
          Just " " -> acc
          Just t ->
            if T.length t == 0 then acc else t : acc
      )
      []
      extractions

getContainedText :: Node -> Maybe Text
getContainedText (Node _ (TEXT t) _) = Just . T.strip $ t
getContainedText (Node _ (CODE t) _) = Just . T.strip $ t
getContainedText (Node _ (LINK _ _) (linkText : _)) = getContainedText linkText
getContainedText (Node _ _ (Node _ (TEXT t) _ : siblings)) =
  Just . joinMaybeTexts $ (Just . T.strip $ t) : (getContainedText <$> siblings)
getContainedText (Node _ _ (Node _ (CODE t) _ : siblings)) =
  Just . joinMaybeTexts $ (Just . T.strip $ t) : (getContainedText <$> siblings)
getContainedText (Node _ _ (Node _ (LINK _ _) (linkText : _) : _)) =
  getContainedText linkText
getContainedText _ = Nothing

checkTableHeader :: Text -> Text -> Node -> Bool
checkTableHeader
  scriptNameColumnName
  scriptDescriptionColumnName
  (Node _ TABLE_ROW (c1 : c2 : _)) =
    getContainedText c1 == Just scriptNameColumnName
      && getContainedText c2 == Just scriptDescriptionColumnName
checkTableHeader _ _ _ = False

checkTable :: Text -> Text -> Node -> Bool
checkTable scriptNameColumnName scriptDescriptionColumnName (Node _ (TABLE _) (hr : _)) =
  checkTableHeader scriptNameColumnName scriptDescriptionColumnName hr
checkTable _ _ _ = False

readScriptDescriptions :: Text -> Text -> Node -> Maybe (M.Map Text Text)
readScriptDescriptions
  scriptNameColumnName
  scriptDescriptionColumnName
  table@(Node _ (TABLE _) _) =
    if checkTable scriptNameColumnName scriptDescriptionColumnName table
      then extractMap table
      else Nothing
readScriptDescriptions _ _ (Node _ _ []) =
  Nothing
readScriptDescriptions
  scriptNameColumnName
  scriptDescriptionColumnName
  (Node _ _ (h : t)) =
    go h t
    where
      go node [] =
        readScriptDescriptions scriptNameColumnName scriptDescriptionColumnName node
      go node (h' : t') =
        case readScriptDescriptions scriptNameColumnName scriptDescriptionColumnName node of
          descs@(Just _) -> descs
          Nothing -> go h' t'
