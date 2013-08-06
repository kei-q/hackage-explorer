{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module View
    ( index
    , searchPackages
    , searchTags
    , taglist
    , tag
    ) where

import Text.Shakespeare.Text
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as T (toLazyText, Builder)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)

import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON (encode)

partial_package :: t -> T.Builder
partial_package = $(textFile "templates/partial/package.ehs")

index, searchPackages, searchTags, taglist, tag :: (ToJSON a) => a -> Text
index          = layout $(textFile "templates/index.ehs") "index"
searchPackages = layout $(textFile "templates/searchPackages.ehs") "search_packages"
searchTags     = layout $(textFile "templates/searchTags.ehs") "search_tags"
taglist        = layout $(textFile "templates/taglist.ehs") "taglist"
tag            = layout $(textFile "templates/tag.ehs") "tag"

layout :: (ToJSON j) => ((t -> t1 -> Text) -> T.Builder) -> Text -> j -> Text
layout content page_title raw = T.toLazyText $ $(textFile "templates/_layout.ehs") dummyRenderUrl
  where json = T.decodeUtf8 $ JSON.encode raw

dummyRenderUrl :: t -> t1 -> Text
dummyRenderUrl _ _ = ""