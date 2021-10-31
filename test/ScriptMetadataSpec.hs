{-# LANGUAGE OverloadedStrings #-}

module ScriptMetadataSpec where

import CMarkGFM (commonmarkToNode, extTable)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import ScriptMetadata (readScriptDescriptions)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Text extractions" $ do
  it "extracts keys from plain text" $
    tableTest tableWithPlainTextName (M.singleton "foo" "bar")
  it "extracts keys from links" $
    tableTest tableWithLinkName (M.singleton "red" "green")
  it "extracts keys from code" $
    tableTest tableWithCodeName (M.singleton "apple" "banana")
  it "extracts keys from code in links" $
    tableTest tableWithCodeLinkName (M.singleton "car" "truck")

tableTest :: Text -> M.Map Text Text -> Expectation
tableTest t m =
  (readScriptDescriptions . commonmarkToNode [] [extTable] $ t) `shouldBe` (Just m)

tableWithPlainTextName :: Text
tableWithPlainTextName = "| Script Name | Description |\n| :---------- | ----------- |\n| foo  | bar |\n"

tableWithLinkName :: Text
tableWithLinkName = "| Script Name | Description |\n| :---------- | ----------- |\n| [red](./scripts/red)  | green |\n"

tableWithCodeName :: Text
tableWithCodeName = "| Script Name | Description |\n| :---------- | ----------- |\n| `apple`  | banana |\n"

tableWithCodeLinkName :: Text
tableWithCodeLinkName = "| Script Name | Description |\n| :---------- | ----------- |\n| [`car`](./scripts/car)  | truck |\n"
