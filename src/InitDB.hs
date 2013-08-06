{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes, KindSignatures, TypeFamilies #-}
module InitDB (run) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql

import qualified Data.Text as T

import Data.Time (getCurrentTime, UTCTime)

import qualified Data.HashMap.Lazy as Map

import qualified Distribution.PackageInfo as DP
import Model
import DB (runDB)

run :: IO ()
run = do
    runDB $ runMigration migrateAll
    latest <- runDB $ selectFirst [] [Desc PackageUpdatedAt]
    case latest of
        Nothing -> runAll
        Just e -> runFrom $ packageUpdatedAt $ entityVal e

runAll :: IO ()
runAll = runFrom $ read "1900-01-01 00:00:00 UTC"

runFrom :: UTCTime -> IO ()
runFrom latest = do
    packages <- DP.fetchLatestPackageInfo "00-index.tar.gz" latest
    liftIO $ print $ "start: count " ++ show (Map.size packages)
    runDB $ mapM_ updatePackageInfo $ Map.elems packages

type UQuery val = forall (m :: * -> *). PersistUnique m => m (Key (val (PersistMonadBackend m)))

updatePackageInfo :: forall (m :: * -> *). (PersistUnique m, PersistQuery m) => DP.Package -> m ()
updatePackageInfo package = do
    pid <- updatePackage package
    tagIds <- mapM insertTag $ DP.categories package
    deleteWhere [PackageTagPackage ==. pid, PackageTagLock ==. True]
    mapM_ (updatePackageCategory pid) tagIds

updatePackage :: DP.Package -> UQuery PackageGeneric
updatePackage package = createOrUpdate $ Package
        { packageName = T.pack $ DP.name package
        , packageVersion = T.pack $ DP.version package
        , packageSynopsis = T.pack $ DP.synopsis package
        , packageUpdatedAt = DP.updatedAt package
        }

--updatePackageCategory :: Key Package -> Key Tag -> m (Key PackageTag)
updatePackageCategory :: forall (m :: * -> *).
                       PersistStore m =>
                       KeyBackend
                         (PersistMonadBackend m) (PackageGeneric (PersistMonadBackend m))
                       -> KeyBackend
                            (PersistMonadBackend m) (TagGeneric (PersistMonadBackend m))
                       -> m (Key (PackageTagGeneric (PersistMonadBackend m)))
updatePackageCategory pid tid = do
    now <- liftIO $ getCurrentTime
    insert $ PackageTag pid tid True now

insertTag :: String -> UQuery TagGeneric
insertTag newTagName = insertBy' $ Tag (T.pack newTagName) ""

-- utils
-- ============================================================================

type HEQuery = forall (m :: * -> *) val. (PersistUnique m, PersistEntity val, PersistMonadBackend m ~ PersistEntityBackend val) => val -> m (Key val)

-- insertBy + keyを返す
insertBy' :: HEQuery
insertBy' val = do
    result <- insertBy val
    return $ case result of
        Right key -> key
        Left entity -> entityKey entity

--
createOrUpdate :: HEQuery
createOrUpdate val = do
    ret <- insertBy val
    case ret of
        Left entity -> let k = entityKey entity in replace k val >> return k
        Right key -> return key

