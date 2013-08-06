{-# LANGUAGE OverloadedStrings #-}
module DB (runDB) where

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)
import qualified Database.Persist.Postgresql as P

-- runDB :: P.SqlPersistT m a -> m a
runDB action = runResourceT $ runNoLoggingT $ P.withPostgresqlConn connectionInfo (P.runSqlConn action)
  where connectionInfo = "host=localhost port=5432 user=hackage dbname=hackage password=hackage"

