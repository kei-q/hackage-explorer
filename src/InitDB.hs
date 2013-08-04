{-# LANGUAGE OverloadedStrings #-}
module InitDB (run) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql

import qualified Data.Text as T

import Data.Time (getCurrentTime)

import qualified Data.HashMap.Lazy as Map

import qualified Distribution.PackageInfo as DP
import Model
import DB (runDB)

run = do
    runDB $ runMigration migrateAll
    latest <- runDB $ selectFirst [] [Desc PackageUpdatedAt]
    runFrom (lastUpdatedAt latest)
  where
    lastUpdatedAt latest = case latest of
        Nothing -> read "1900-01-01 00:00:00 UTC"
        Just e -> packageUpdatedAt $ entityVal e

runAll = runFrom $ read "1900-01-01 00:00:00 UTC"

runFrom latest = do
    packages <- DP.fetchLatestPackageInfo "00-index.tar.gz" latest
    liftIO $ print $ "start: count " ++ show (Map.size packages)
    runDB $ mapM_ updatePackageInfo $ Map.elems packages

updatePackageInfo package = do
    pid <- updatePackage package
    tagIds <- mapM insertTag $ DP.categories package
    deleteWhere [PackageTagPackage ==. pid, PackageTagLock ==. True]
    mapM_ (updatePackageCategory pid) tagIds

updatePackage package = createOrUpdate $ Package
        { packageName = T.pack $ DP.name package
        , packageVersion = T.pack $ DP.version package
        , packageSynopsis = T.pack $ DP.synopsis package
        , packageUpdatedAt = DP.updatedAt package
        }

updatePackageCategory pid tid = do
    now <- liftIO $ getCurrentTime
    insert $ PackageTag pid tid True now

insertTag tagName = insertBy' $ mkTag $ T.pack tagName
  where mkTag name = Tag name ""

-- utils
-- ============================================================================

-- insertBy + keyを返す
insertBy' val = do
    result <- insertBy val
    return $ case result of
        Right key -> key
        Left entity -> entityKey entity

--
createOrUpdate val = do
    ret <- insertBy val
    case ret of
        Left entity -> let k = entityKey entity in replace k val >> return k
        Right key -> return key

