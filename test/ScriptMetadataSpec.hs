{-# LANGUAGE OverloadedStrings #-}

module ScriptMetadataSpec where

import CMarkGFM (Node (Node), NodeType (DOCUMENT), commonmarkToNode, extTable)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import ScriptMetadata (getContainedText, readScriptDescriptions)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Table key extractions" $ do
    it "extracts keys from plain text" $
      tableTest tableWithPlainTextName (M.singleton "foo" "bar")
    it "extracts keys from links" $
      tableTest tableWithLinkName (M.singleton "red" "green")
    it "extracts keys from code" $
      tableTest tableWithCodeName (M.singleton "apple" "banana")
    it "extracts keys from code in links" $
      tableTest tableWithCodeLinkName (M.singleton "car" "truck")
    it "doesn't extract anything from tables with unexpected column headings" $
      (readScriptDescriptions . commonmarkToNode [] [extTable] $ badHeadingsText) `shouldBe` Nothing
    it "ignores bad rows but collects good ones" $
      tableTest tableWithABadRow (M.fromList [("foo", "bar"), ("car", "truck")])
  describe "Text extractions" $ do
    it "extracts text from plain text" $
      textTest plainText "foo"
    it "extracts text from code" $
      textTest codeText "bar"
    it "extracts text from links" $
      textTest linkText "baz"
    it "extracts text from code in a link" $
      textTest linkCodeText "qux"

tableTest :: Text -> M.Map Text Text -> Expectation
tableTest t m =
  (readScriptDescriptions . commonmarkToNode [] [extTable] $ t) `shouldBe` (Just m)

textTest :: Text -> Text -> Expectation
textTest markdownText expectedText =
  let (Node _ DOCUMENT (firstChild : _)) = commonmarkToNode [] [extTable] markdownText
   in getContainedText firstChild `shouldBe` (Just expectedText)

tableWithPlainTextName :: Text
tableWithPlainTextName = "| Script Name | Description |\n| :---------- | ----------- |\n| foo  | bar |\n"

tableWithLinkName :: Text
tableWithLinkName = "| Script Name | Description |\n| :---------- | ----------- |\n| [red](./scripts/red)  | green |\n"

tableWithCodeName :: Text
tableWithCodeName = "| Script Name | Description |\n| :---------- | ----------- |\n| `apple`  | banana |\n"

tableWithCodeLinkName :: Text
tableWithCodeLinkName = "| Script Name | Description |\n| :---------- | ----------- |\n| [`car`](./scripts/car)  | truck |\n"

plainText :: Text
plainText = "foo"

codeText :: Text
codeText = "`bar`"

linkText :: Text
linkText = "[baz](./scripts/baz)"

linkCodeText :: Text
linkCodeText = "[`qux`](./scripts/qux)"

badHeadingsText :: Text
badHeadingsText = "| no good | bogus |\n| :----- | ----- |\n| foo | bar |"

tableWithABadRow :: Text
tableWithABadRow = "| Script Name | Description |\n| :---------- | ----------- |\n| truck |\n| foo  | bar |\n| [`car`](./scripts/car)  | truck |\n"
