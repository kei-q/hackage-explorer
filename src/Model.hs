{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import Import
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Package json
    name Text
    version Text
--    license DL.License
--    homepage Text
--    bugReports Text
--    sourceRepo (Maybe (DPD.RepoType, Text))
    synopsis Text
--    description Text
--    categories [Text]
--    hasExes Bool
--    hasLibs Bool
--    hasTests Bool
--    hasBenchmarks Bool
    updatedAt UTCTime
    UniquePackageName name
    deriving Eq Show

Tag json
    name Text
    synopsis Text
    UniqueTagName name
    deriving Show

PackageTag
    package PackageId
    tag TagId
    lock Bool
    createdAt UTCTime default=now()
    UniquePT package tag
    deriving Show
|]
