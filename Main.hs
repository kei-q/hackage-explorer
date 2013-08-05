module Main(main) where

import System.Environment (getArgs)

import qualified Server
import qualified InitDB

main :: IO ()
main = do
    args <- getArgs
    if null args
        then Server.run 3000
        else InitDB.run