{-# LANGUAGE OverloadedStrings #-}
module Model.Package
    ( getAll
    , getUpdated
    , getLatest
    , search
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON

import qualified Data.List as List

import ImportDB
import Model

type PackageGetter = Page -> Limit -> SqlQuery (SqlExpr (Value PackageId))

packageQuery :: Page -> Limit -> SqlExpr (Entity Package) -> SqlQuery (SqlExpr (Value PackageId))
packageQuery page lim p = do
    offset $ (page-1) * lim
    limit lim
    return (p ^. PackageId)


getAll :: Text -> Page -> SqlM [JSON.Value]
getAll t p = getPackages (getAllQuery t p 10)

getAllQuery :: Text -> PackageGetter
getAllQuery tag page lim = from $ \(p,pt,t) -> do
    where_ (t ^. TagName ==. val tag)
    where_ (pt ^. PackageTagTag ==. t ^. TagId)
    where_ (pt ^. PackageTagPackage ==. p ^. PackageId)
    orderBy [desc (p ^. PackageUpdatedAt)]
    packageQuery page lim p


getUpdated :: Page -> SqlM [JSON.Value]
getUpdated p = getPackages (updatedQuery p 5)

updatedQuery :: PackageGetter
updatedQuery page lim = from $ \(p `InnerJoin` pt) -> do
    on (pt ^. PackageTagPackage ==. p ^. PackageId)
    orderBy [desc (pt ^. PackageTagCreatedAt)]
    packageQuery page lim p


getLatest :: Page -> SqlM [JSON.Value]
getLatest p = getPackages (latestPackagesQuery p 5)

latestPackagesQuery :: PackageGetter
latestPackagesQuery page lim = from $ \p -> do
    orderBy [desc (p ^. PackageUpdatedAt)]
    packageQuery page lim p


search :: Text -> Page -> SqlM [JSON.Value]
search keyword p = getPackages (searchQuery keyword p 10)

searchQuery :: Text -> PackageGetter
searchQuery keyword page lim = from $ \p -> do
    let k = val keyword
    where_ $ (p ^. PackageName @@. k) ||. (p ^. PackageSynopsis @@. k)
    orderBy [desc (p ^. PackageUpdatedAt)]
    packageQuery page lim p


getPackages :: SqlQuery (SqlExpr (Value PackageId)) -> SqlM [JSON.Value]
getPackages packagesQuery = do
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
