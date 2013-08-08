{-# LANGUAGE OverloadedStrings #-}
module Server(run) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Network.HTTP.Types (notFound404)
import Web.Scotty

import qualified View

import qualified Model.Package as Package
import qualified Model.Tag as Tag
import qualified Model.PackageTag as PackageTag
import DB (runDB)
import ImportDB (SqlM, Int64, Text)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

runDB' :: SqlM a -> ActionM a
runDB' = liftIO . runDB

r404 :: ActionM ()
r404 = do
    status notFound404
    text "404"

instance Parsable Text where
    parseParam t = Right $ T.toStrict t

instance Parsable Int64 where
    parseParam t = case T.decimal t of
        Left e -> Left $ T.pack e
        Right (n,_) -> Right n

run :: Int -> IO ()
run port = scotty port $ do
    middleware static
    middleware logStdoutDev

    -- create package_tag
    post "/packages/tags/new" $ rescueJSON $ do
        target <- jsonData
        runDB' (PackageTag.setTag target) >>= json

    -- delete package_tag
    post "/packages/tags/delete" $ rescueJSON $ do
        (key:_) <- jsonData -- TODO: pertial
        count <- runDB' $ PackageTag.deleteTag key
        json $ count == 1

    -- package
    -- =========================================================================
    get "/" $ do
        updated <- runDB' $ Package.getUpdated 1
        latest <- runDB' $ Package.getLatest 1
        html $ View.index (updated, latest)

    get "/updated/:page" $ \page -> do
        runDB' (Package.getUpdated page) >>= json

    get "/latest/:page" $ \page -> do
        runDB' (Package.getLatest page) >>= json

    get "/search/packages" $ do
        keyword <- param "keyword"
        runDB' (Package.search keyword 1) >>= html . View.searchPackages

    get "/search/packages/:page" $ \page -> do
        keyword <- param "keyword"
        runDB' (Package.search keyword page) >>= json


    -- tag
    -- =========================================================================
    get "/taglist" $ runDB' (Tag.getAll 20 1) >>= html . View.taglist
    get "/taglist/:page" $ \page -> runDB' (Tag.getAll 20 page) >>= json

    get "/tags/:tag" $ \key -> do
        tag <- runDB' $ Tag.getTag key
        case tag of
            Nothing -> r404
            Just t -> do
                packages <- runDB' $ Package.getAll key 1
                html $ View.tag (t,packages)

    get "/tags/:tag/:page" $ \tag page -> do
        runDB' (Package.getAll tag page) >>= json

    get "/search/tags" $ do
        keyword <- param "keyword"
        runDB' (Tag.search keyword 1) >>= html . View.searchTags

    get "/search/tags/:page" $ \page -> do
        keyword <- param "keyword"
        runDB' (Tag.search keyword page) >>= json

    -- update tag synopsis
    post "/tag/synopsis" $ rescueJSON $ do
        target <- jsonData
        runDB' $ Tag.updateSynopsis target
        json True

rescueJSON :: ActionM () -> ActionM ()
rescueJSON action = action `rescue` \msg -> do
    liftIO $ print msg
    json False
