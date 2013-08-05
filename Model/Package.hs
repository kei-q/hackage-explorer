{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Package
    ( getPackages
    , getLatestPackages
    , latestPackagesQuery
    , getPackages'
    ) where

import Database.Esqueleto

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON

import qualified Data.List as List
import Data.Text (Text)
import Data.Int (Int64)

import Model
import DB (runDB)

type Page = Int64

instance (JSON.ToJSON a) => JSON.ToJSON (Value a) where
  toJSON (Value a) = JSON.toJSON a

getPackages :: Text -> Page -> IO [JSON.Value]
getPackages t p = getPackages' (getPackagesQuery t p 10)

getPackagesQuery tagName page lim = from $ \(p,pt,t) -> do
    where_ (t ^. TagName ==. val tagName)
    where_ (pt ^. PackageTagTag ==. t ^. TagId)
    where_ (pt ^. PackageTagPackage ==. p ^. PackageId)
    orderBy [desc (p ^. PackageUpdatedAt)]
    offset $ (page-1) * lim
    limit lim
    return (p ^. PackageId)



getLatestPackages :: Page -> IO [JSON.Value]
getLatestPackages p = getPackages' (latestPackagesQuery p 10)

latestPackagesQuery page lim = from $ \p -> do
    orderBy [desc (p ^. PackageUpdatedAt)]
    offset $ (page-1) * lim
    limit lim
    return (p ^. PackageId)



getPackages' packagesQuery = runDB $ do
  ret <- select $ from $ \(p `InnerJoin` pt `InnerJoin` t) -> do
    on (pt ^. PackageTagTag ==. t ^. TagId)
    on (pt ^. PackageTagPackage ==. p ^. PackageId)
    where_ (pt ^. PackageTagPackage `in_` subList_select packagesQuery)
    orderBy [desc (p ^. PackageUpdatedAt)]
    return (p, (pt ^. PackageTagId, t ^. TagName, pt ^. PackageTagLock))
  return $ map bar $ hoge $ map (\(p,ts) -> (p, foo ts)) ret
    where
      foo (k,s,l) = JSON.object ["id" .= k, "name" .= s, "lock" .= l]
      bar (p,ts) = JSON.object ["package" .= p, "tags" .= ts]

hoge :: Eq a => [(a,b)] -> [(a, [b])]
hoge xs = map aux $ List.groupBy (\a b -> fst a == fst b) xs
  where aux a@(x:_) = (fst x, map snd a)
