{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Package where

import Database.Esqueleto

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON

import qualified Data.List as List

import Model
import DB (runDB)

instance (JSON.ToJSON a) => JSON.ToJSON (Value a) where
  toJSON (Value a) = JSON.toJSON a

getLatestPackages :: IO [JSON.Value]
getLatestPackages = getPackages' (latestPackagesQuery 20)

latestPackagesQuery lim = from $ \p -> do
    orderBy [desc (p ^. PackageUpdatedAt)]
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
