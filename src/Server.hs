{-# LANGUAGE OverloadedStrings #-}
module Server(run) where

--import Control.Monad.IO.Class (liftIO)
--import Network.Wai.Middleware.Static (static)

import Web.Scotty
--import qualified Data.Aeson as JSON
--import Control.Applicative

--import qualified Template
--import qualified Query

run :: Int -> IO ()
run port = scotty port $ do
    get "/" $ do
        text "hello"
    get "/test" $ do
        text "piyo"
