{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module View where

import Text.Shakespeare.Text
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText, Builder)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String (IsString)

import qualified Data.Aeson as JSON

partial_package :: t -> Builder
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

searchTags :: (JSON.ToJSON a) => a -> Text
searchTags raw = layout $(textFile "templates/searchTags.ehs")
  where
    json = decodeUtf8 $ JSON.encode raw
    page_title :: Text
    page_title = "search_tags"

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

layout :: IsString a => ((t -> t1 -> a) -> Builder) -> Text
layout content = toLazyText $ $(textFile "templates/_layout.ehs") dummyRenderUrl

dummyRenderUrl :: IsString a => t -> t1 -> a
dummyRenderUrl _ _ = ""