module Distribution.PackageInfo
    ( fetchPackageInfo
    ) where

import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parse as PDP
import qualified Distribution.License as DL
import qualified Distribution.Text as DT

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString.Lazy as BL

import Data.Time (UTCTime)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import qualified Codec.Compression.GZip as GZip (decompress)

-- for utils
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Exception (throw)

import Data.Maybe (catMaybes)

import qualified Distribution.Category as DC

data Package = Package
    { name :: String
    , version :: String
    , license :: DL.License

    -- urls
    , homepage :: String
    , bugReports :: String

    , sourceRepos :: [PD.SourceRepo]
    , synopsis :: String
    , description :: String
    , categories :: [String]

    , hasExes :: Bool
    , hasLibs :: Bool
    , hasTests :: Bool
    , hasBenchmarks :: Bool

    , updatedAt :: UTCTime
    } deriving (Show)

-- utils
-- =============================================================================

eToU :: Tar.EpochTime -> UTCTime
eToU = posixSecondsToUTCTime . realToFrac

entriesToList :: Tar.Entries Tar.FormatError -> [Tar.Entry]
entriesToList Tar.Done = []
entriesToList (Tar.Fail s) = throw s
entriesToList (Tar.Next e es) = e : entriesToList es

filterNomalFiles :: [Tar.Entry] -> [Tar.Entry]
filterNomalFiles = filter (isNomalFile . Tar.entryContent)

isNomalFile :: Tar.EntryContent -> Bool
isNomalFile (Tar.NormalFile _ _) = True
isNomalFile _ = False

-- main
-- =============================================================================

fetchPackageInfo :: FilePath -> IO [Package]
fetchPackageInfo tarName = do
    contents <- BL.readFile tarName -- "00-index.tar.gz"
    return
        $ catMaybes
        $ map readPackageDescription
        $ filterNomalFiles
        $ entriesToList
        $ Tar.read
        $ GZip.decompress contents

readPackageDescription :: Tar.Entry -> Maybe Package
readPackageDescription entry = fmap pack pd
  where
    pack pd = Package {
        name            = DT.display $ P.pkgName $ PD.package pd
        , version       = DT.display $ PD.specVersion pd
        , license       = PD.license pd
        , homepage      = PD.homepage pd
        , bugReports    = PD.bugReports pd
        , sourceRepos   = PD.sourceRepos pd
        , synopsis      = PD.synopsis pd
        , description   = PD.description pd
        , categories    = DC.parse $ PD.category pd
        , hasExes       = PD.hasExes pd
        , hasLibs       = PD.hasLibs pd
        , hasTests      = PD.hasTests pd
        , hasBenchmarks = PD.hasBenchmarks pd
        , updatedAt     = eToU $ Tar.entryTime entry
    }
    (Tar.NormalFile content _) = Tar.entryContent entry
    pd = case PDP.parsePackageDescription $ T.unpack $ T.decodeUtf8With T.lenientDecode content of
        PDP.ParseOk _ x -> Just $ PD.packageDescription x
        _ -> Nothing


