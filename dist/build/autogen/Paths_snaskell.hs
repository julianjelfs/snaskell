{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_snaskell (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/julianj/.cabal/bin"
libdir     = "/home/julianj/.cabal/lib/x86_64-linux-ghc-8.0.2/snaskell-0.1.0.0-26sFDPzryEm1NrfigNmhou"
dynlibdir  = "/home/julianj/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/julianj/.cabal/share/x86_64-linux-ghc-8.0.2/snaskell-0.1.0.0"
libexecdir = "/home/julianj/.cabal/libexec"
sysconfdir = "/home/julianj/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "snaskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "snaskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "snaskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "snaskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "snaskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "snaskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
