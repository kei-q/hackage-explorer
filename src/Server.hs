{-# LANGUAGE OverloadedStrings #-}
module Server(run) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static (static)

import Web.Scotty

import qualified Model.Package
import qualified View

run :: Int -> IO ()
run port = scotty port $ do
    middleware static

    get "/" $ do
        packages <- liftIO $ Model.Package.getLatestPackages
        html $ View.index packages
