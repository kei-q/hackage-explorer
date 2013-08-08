{-# LANGUAGE OverloadedStrings #-}
module Model.PackageTag where

import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)

import ImportDB
import Model

-- post
-- =============================================================================

setTag :: (PackageId, Text) -> SqlM PackageTagId
setTag (pid, newTag) = do
  tid <- insertTag newTag
  now <- liftIO $ getCurrentTime
  insert $ PackageTag pid tid False now

insertTag :: Text -> SqlM TagId
insertTag tag = do
  result <- insertBy (mkTag tag)
  return $ case result of
      Right key -> key
      Left entity -> entityKey entity
  where mkTag name = Tag name ""

-- delete
-- =============================================================================

deleteTag :: PackageTagId -> SqlM Int64
deleteTag ptid = do
  deleteCount $ from $ \pt -> do
    where_ (pt ^. PackageTagId ==. val ptid)
    where_ (pt ^. PackageTagLock ==. val False)
