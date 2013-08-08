{-# LANGUAGE OverloadedStrings #-}
module Server(run) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Network.HTTP.Types (notFound404)
import Web.Scotty

import Control.Applicative
import qualified Data.Text as Text

import qualified Model.Package
import qualified Model.PackageTag
import qualified View

import qualified Model.Tag as Tag
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

    get "/" $ do
        packages <- liftIO $ (,) <$> Model.Package.getUpdated 1 <*> Model.Package.getLatestPackages 1
        html $ View.index packages

    get "/updated/:page" $ \page -> do
        packages <- liftIO $ Model.Package.getUpdated (read page)
        json packages

    get "/latest/:page" $ \page -> do
        packages <- liftIO $ Model.Package.getLatestPackages (read page)
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

    -- tag
    -- =========================================================================
    get "/taglist" $ do
        tags <- runDB' $ Tag.getAll 20 1
        html $ View.taglist tags

    get "/taglist/:page" $ \page -> do
        tags <- runDB' $ Tag.getAll 20 page
        json tags

    get "/tags/:tag" $ \key -> do
        tag <- runDB' $ Tag.getTag key
        case tag of
            Nothing -> r404
            Just t -> do
                packages <- liftIO $ Model.Package.getPackages key 1
                html $ View.tag (t,packages)

    get "/tags/:tag/:page" $ \tag page -> do
        let tag' = Text.pack tag
        packages <- liftIO $ Model.Package.getPackages tag' page
        json packages

    get "/search/tags" $ do
        keyword <- param "keyword"
        target <- runDB' $ Tag.search keyword 1
        html $ View.searchTags target

    get "/search/tags/:page" $ \page -> do
        keyword <- param "keyword"
        tags <- runDB' $ Tag.search keyword page
        json tags

    -- update tag synopsis
    post "/tag/synopsis" $ rescueJSON $ do
        target <- jsonData
        runDB' $ Tag.updateSynopsis target
        json True

rescueJSON :: ActionM () -> ActionM ()
rescueJSON action = action `rescue` \msg -> do
    liftIO $ print msg
    json False
