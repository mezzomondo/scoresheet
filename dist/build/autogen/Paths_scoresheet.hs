module Paths_scoresheet (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mikifossati/Library/Haskell/bin"
libdir     = "/Users/mikifossati/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support/lib/ghc/scoresheet-0.1.0.0"
datadir    = "/Users/mikifossati/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support/share/scoresheet-0.1.0.0"
libexecdir = "/Users/mikifossati/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support/libexec"
sysconfdir = "/Users/mikifossati/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "scoresheet_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "scoresheet_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "scoresheet_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "scoresheet_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "scoresheet_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
