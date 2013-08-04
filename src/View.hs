{-# LANGUAGE TemplateHaskell #-}
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
    page_title = "index"

layout content = toLazyText $ $(textFile "templates/_layout.ehs") ()
