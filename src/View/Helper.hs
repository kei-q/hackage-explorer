{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module View.Helper where

import Text.Shakespeare.Text
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText, Builder)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String (IsString)
import Data.Monoid ((<>))

import qualified Data.Aeson as JSON

--load :: (JSON.ToJSON a) => a -> Text
load page_title raw = layout $(textFile "templates/" <> page_title <> ".ehs")
  where
    json = decodeUtf8 $ JSON.encode raw

layout :: IsString a => ((t -> t1 -> a) -> Builder) -> Text
layout content = toLazyText $ $(textFile "templates/_layout.ehs") dummyRenderUrl

dummyRenderUrl :: IsString a => t -> t1 -> a
dummyRenderUrl _ _ = ""