{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Tag where

import Database.Esqueleto
import qualified Data.Aeson as JSON
import Model
import GHC.Generics (Generic)

import DB (runDB)

data TagInfo = TagInfo
  { tag :: Entity Tag
  , count :: Value Int
  } deriving Generic

instance JSON.ToJSON TagInfo

allTags :: IO [TagInfo]
allTags = runDB $ do
    ret <- select $ from $ \(pt `InnerJoin` t) -> do
        on (pt ^. PackageTagTag ==. t ^. TagId)
        groupBy $ t ^. TagId
        let c = Database.Esqueleto.count (t ^. TagId)
        orderBy [desc c]
        limit 20
        return (t, c)
    return $ map (uncurry TagInfo) ret

