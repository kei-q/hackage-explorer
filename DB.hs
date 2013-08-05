{-# LANGUAGE OverloadedStrings #-}
module DB (runDB) where

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)
import qualified Database.Persist.Postgresql as P

connectionInfo = "host=localhost port=5432 user=hackage dbname=hackage password=hackage"
runDB action = runResourceT $ runNoLoggingT $ P.withPostgresqlConn connectionInfo (P.runSqlConn action)

