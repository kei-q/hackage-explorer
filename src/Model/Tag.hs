{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-} -- for Hastache
module Model.Tag where

import Database.Esqueleto
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)
import qualified Data.Aeson as JSON
import Model
import Data.Text (Text)
import Data.Int (Int64)
import GHC.Generics (Generic)

import DB (runDB)

type Page = Int64

instance (JSON.ToJSON a) => JSON.ToJSON (Value a) where
  toJSON (Value a) = JSON.toJSON a

data TagInfo = TagInfo
  { tag :: Entity Tag
  , count :: Value Int
  } deriving Generic

instance JSON.ToJSON TagInfo

allTags :: Page -> IO [TagInfo] -- [(Entity Tag, Value Count)]
allTags page = runDB $ do
    let lim = 10
    ret <- select $ from $ \(pt `InnerJoin` t) -> do
        on (pt ^. PackageTagTag ==. t ^. TagId)
        groupBy $ t ^. TagId
        let countRows' = countRows
        orderBy [desc countRows']
        offset $ (page-1) * lim
        limit lim
        return (t, countRows')
    return $ map (uncurry TagInfo) ret

getTag :: Text -> IO (Entity Tag)
getTag tagName = runDB $ do
  tags <- select $ from $ \t -> do
    where_ (t ^. TagName ==. val tagName)
    return t
  return $ head tags -- TODO: partial

-- search
-- --------------------------------------------------------

(@@.) :: SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value Bool)
(@@.) = unsafeSqlBinOp " @@ "

search :: Text -> Page -> IO [TagInfo] -- [(Entity Tag, Value Count)]
search keyword page = runDB $ do
    let lim = 10
    ret <- select $ from $ \(pt `InnerJoin` t) -> do
        on (pt ^. PackageTagTag ==. t ^. TagId)
        let k = val keyword
        where_ ((t ^. TagName @@. k) ||. (t ^. TagSynopsis @@. k))
        groupBy $ t ^. TagId
        let countRows' = countRows
        orderBy [desc countRows']
        offset $ (page-1) * lim
        limit lim
        return (t, countRows')
    return $ map (uncurry TagInfo) ret