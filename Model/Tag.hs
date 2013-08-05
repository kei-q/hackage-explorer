{-# LANGUAGE OverloadedStrings #-}
module Model.Tag where

import Database.Esqueleto
import qualified Data.Aeson as JSON
import Model
import Data.Text (Text)

import DB (runDB)

instance (JSON.ToJSON a) => JSON.ToJSON (Value a) where
  toJSON (Value a) = JSON.toJSON a

getTag :: Text -> IO (Entity Tag)
getTag tagName = runDB $ do
  tags <- select $ from $ \t -> do
    where_ (t ^. TagName ==. val tagName)
    return t
  return $ head tags -- TODO: partial
