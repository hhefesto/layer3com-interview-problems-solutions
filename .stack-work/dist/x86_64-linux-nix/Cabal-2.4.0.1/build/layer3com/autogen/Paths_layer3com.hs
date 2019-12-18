{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_layer3com (
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

bindir     = "/home/hhefesto/src/layer3com/layer3com-interview-problems-solutions/.stack-work/install/x86_64-linux-nix/lts-14.17/8.6.5/bin"
libdir     = "/home/hhefesto/src/layer3com/layer3com-interview-problems-solutions/.stack-work/install/x86_64-linux-nix/lts-14.17/8.6.5/lib/x86_64-linux-ghc-8.6.5/layer3com-0.1.0.0-8JUwK5K4FNZLQirUyYDir6-layer3com"
dynlibdir  = "/home/hhefesto/src/layer3com/layer3com-interview-problems-solutions/.stack-work/install/x86_64-linux-nix/lts-14.17/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/hhefesto/src/layer3com/layer3com-interview-problems-solutions/.stack-work/install/x86_64-linux-nix/lts-14.17/8.6.5/share/x86_64-linux-ghc-8.6.5/layer3com-0.1.0.0"
libexecdir = "/home/hhefesto/src/layer3com/layer3com-interview-problems-solutions/.stack-work/install/x86_64-linux-nix/lts-14.17/8.6.5/libexec/x86_64-linux-ghc-8.6.5/layer3com-0.1.0.0"
sysconfdir = "/home/hhefesto/src/layer3com/layer3com-interview-problems-solutions/.stack-work/install/x86_64-linux-nix/lts-14.17/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "layer3com_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "layer3com_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "layer3com_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "layer3com_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "layer3com_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "layer3com_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
