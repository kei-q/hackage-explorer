{-# LANGUAGE OverloadedStrings #-}
module Server(run) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai (Application)

import Web.Scotty

import Control.Applicative
import qualified Data.Text as Text

import qualified Model.Tag
import qualified Model.Package
import qualified Model.PackageTag
import qualified View

run :: Int -> IO ()
run port = scotty port $ do
    middleware static
    middleware logStdoutDev

    get "/" $ do
        packages <- liftIO $ (,) <$> Model.Package.getUpdated 1 <*> Model.Package.getLatestPackages 1
        html $ View.index packages

    get "/updated/:page" $ \page -> do
        packages <- liftIO $ Model.Package.getUpdated (read page)
        json packages

    get "/latest/:page" $ \page -> do
        packages <- liftIO $ Model.Package.getLatestPackages (read page)
        json packages

    get "/taglist" $ do
        tags <- liftIO $ Model.Tag.allTags 1
        html $ View.taglist tags

    get "/taglist/:page" $ \page -> do
        tags <- liftIO $ Model.Tag.allTags (read page)
        json tags

    get "/tags/:tag" $ \key -> do
        let key' = Text.pack key
        target <- liftIO $ (,) <$> Model.Tag.getTag key' <*> Model.Package.getPackages key' 1
        html $ View.tag target

    get "/tags/:tag/:page" $ \tag page -> do
        let tag' = Text.pack tag
        packages <- liftIO $ Model.Package.getPackages tag' (read page)
        json packages

    -- search
    get "/search/packages" $ do
        keyword <- param "keyword"
        target <- liftIO $ Model.Package.search (Text.pack keyword) 1
        html $ View.searchPackages target

    get "/search/packages/:page" $ \page -> do
        keyword <- param "keyword"
        packages <- liftIO $ Model.Package.search (Text.pack keyword) (read page)
        json packages

    get "/search/tags" $ do
        keyword <- param "keyword"
        target <- liftIO $ Model.Tag.search (Text.pack keyword) 1
        html $ View.searchTags target

    get "/search/tags/:page" $ \page -> do
        keyword <- param "keyword"
        tags <- liftIO $ Model.Tag.search (Text.pack keyword) (read page)
        json tags

    -- create package_tag
    post "/packages/tags/new" $ rescueJSON $ do
        target <- jsonData
        tag <- liftIO $ Model.PackageTag.setTag target
        json tag

    -- delete package_tag
    post "/packages/tags/delete" $ rescueJSON $ do
        (key:_) <- jsonData -- TODO: pertial
        count <- liftIO $ Model.PackageTag.deleteTag key
        json $ count == 1

    -- update tag synopsis
    post "/tag/synopsis" $ rescueJSON $ do
        target <- jsonData
        liftIO $ Model.Tag.updateSynopsis target
        json True

rescueJSON action = action `rescue` \msg -> do
    liftIO $ print msg
    json False
