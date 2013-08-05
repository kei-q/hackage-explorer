{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module View where

import Text.Shakespeare
import Text.Shakespeare.Text
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.Aeson as JSON

partial_package = $(textFile "templates/partial/package.ehs")

index :: (JSON.ToJSON a) => a -> Text
index raw = layout $(textFile "templates/index.ehs")
  where
    json = decodeUtf8 $ JSON.encode raw
    page_title :: Text
    page_title = "index"

searchPackages :: (JSON.ToJSON a) => a -> Text
searchPackages raw = layout $(textFile "templates/searchPackages.ehs")
  where
    json = decodeUtf8 $ JSON.encode raw
    page_title :: Text
    page_title = "search_packages"

taglist :: (JSON.ToJSON a) => a -> Text
taglist raw = layout $(textFile "templates/taglist.ehs")
  where
    json = decodeUtf8 $ JSON.encode raw
    page_title :: Text
    page_title = "taglist"

tag :: (JSON.ToJSON a) => a -> Text
tag raw = layout $(textFile "templates/tag.ehs")
  where
    json = decodeUtf8 $ JSON.encode raw
    page_title :: Text
    page_title = "tag"

layout content = toLazyText $ $(textFile "templates/_layout.ehs") dummyRenderUrl

dummyRenderUrl _ _ = ""