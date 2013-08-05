{-# LANGUAGE OverloadedStrings #-}
module Server(run) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static (static)
import Network.Wai (Application)

import Web.Scotty

import qualified Model.Package
import qualified Model.PackageTag
import qualified View

run :: Int -> IO ()
run port = scotty port $ do
    middleware static

    get "/" $ do
        packages <- liftIO $ Model.Package.getLatestPackages 1
        liftIO $ print $ head packages
        html $ View.index packages

    get "/latest/:page" $ \page -> do
        packages <- liftIO $ Model.Package.getLatestPackages (read page)
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

rescueJSON action = action `rescue` \msg -> do
    liftIO $ print msg
    json False
