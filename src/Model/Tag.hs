{-# LANGUAGE OverloadedStrings #-}
module Model.Tag
    ( getTag
    , getAll
    , search
    , updateSynopsis
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON

import ImportDB
import Model

getTag :: Text -> SqlM (Maybe (Entity Tag))
getTag t = getBy $ UniqueTagName t

getAll :: Limit -> Page -> SqlM [JSON.Value]
getAll lim page = getTags page lim $ \pt t -> return ()

search :: Text -> Page -> SqlM [JSON.Value]
search keyword page = do
    getTags page 10 $ \pt t -> do
        let k = val keyword
        where_ ((t ^. TagName @@. k) ||. (t ^. TagSynopsis @@. k))

updateSynopsis :: (TagId, Text) -> SqlM ()
updateSynopsis (tid, synopsis) = do
  update $ \t -> do
    set t [TagSynopsis =. val synopsis]
    where_ (t ^. TagId ==. val tid)

getTags :: Page -> Limit
        -> (SqlExpr (Entity PackageTag) -> SqlExpr (Entity Tag) -> SqlQuery t)
        -> SqlM [JSON.Value]
getTags page lim predicate = do
    ret <- select $ from $ \(pt `InnerJoin` t) -> do
        on (pt ^. PackageTagTag ==. t ^. TagId)
        _ <- predicate pt t
        groupBy $ t ^. TagId
        let countRows' = countRows
        orderBy [desc countRows']
        offset $ (page-1) * lim
        limit lim
        return (t, countRows')
    return $ map json ret
    where
      json :: (Entity Tag, Value Int) -> JSON.Value
      json (t, c) = JSON.object ["tag" .= t, "count" .= c]
