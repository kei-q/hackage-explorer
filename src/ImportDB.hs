{-# LANGUAGE OverloadedStrings #-}
module ImportDB
    ( module Database.Esqueleto
    , module Import
    , (@@.)
    , Page
    , Limit
    , SqlM
    ) where

import Import

import Database.Esqueleto
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)

import Database.Persist.Sql (SqlPersistT)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Resource (ResourceT)

import qualified Data.Aeson as JSON

instance (JSON.ToJSON a) => JSON.ToJSON (Value a) where
  toJSON (Value a) = JSON.toJSON a

(@@.) :: SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value Bool)
(@@.) = unsafeSqlBinOp " @@ "

type Page = Int64
type Limit = Int64

type SqlM = SqlPersistT (LoggingT (ResourceT IO))