{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.PackageTag where

import Import

import Database.Esqueleto
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)

import Model
import DB (runDB)

-- post
-- =============================================================================

setTag :: (PackageId, Text) -> IO PackageTagId
setTag (pid, newTag) = runDB $ do
  tid <- insertTag newTag
  now <- liftIO $ getCurrentTime
  insert $ PackageTag pid tid False now

-- insertTag :: Text -> m TagId
insertTag tag = do
  result <- insertBy (mkTag tag)
  return $ case result of
      Right key -> key
      Left entity -> entityKey entity
  where mkTag name = Tag name ""

-- delete
-- =============================================================================

deleteTag :: PackageTagId -> IO Int64
deleteTag ptid = runDB $ do
  deleteCount $ from $ \pt -> do
    where_ (pt ^. PackageTagId ==. val ptid)
    where_ (pt ^. PackageTagLock ==. val False)
