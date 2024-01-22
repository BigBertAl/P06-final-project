{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_huffman (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\alber\\Desktop\\Hochschule\\7.Semester\\Anwendungsentwicklung\\Praktikum\\P06-final-project\\code\\.stack-work\\install\\1dd550c8\\bin"
libdir     = "C:\\Users\\alber\\Desktop\\Hochschule\\7.Semester\\Anwendungsentwicklung\\Praktikum\\P06-final-project\\code\\.stack-work\\install\\1dd550c8\\lib\\x86_64-windows-ghc-9.4.7\\huffman-0.1-7oGsPwnMV4MIdxe92R26Zp-huffman-exe"
dynlibdir  = "C:\\Users\\alber\\Desktop\\Hochschule\\7.Semester\\Anwendungsentwicklung\\Praktikum\\P06-final-project\\code\\.stack-work\\install\\1dd550c8\\lib\\x86_64-windows-ghc-9.4.7"
datadir    = "C:\\Users\\alber\\Desktop\\Hochschule\\7.Semester\\Anwendungsentwicklung\\Praktikum\\P06-final-project\\code\\.stack-work\\install\\1dd550c8\\share\\x86_64-windows-ghc-9.4.7\\huffman-0.1"
libexecdir = "C:\\Users\\alber\\Desktop\\Hochschule\\7.Semester\\Anwendungsentwicklung\\Praktikum\\P06-final-project\\code\\.stack-work\\install\\1dd550c8\\libexec\\x86_64-windows-ghc-9.4.7\\huffman-0.1"
sysconfdir = "C:\\Users\\alber\\Desktop\\Hochschule\\7.Semester\\Anwendungsentwicklung\\Praktikum\\P06-final-project\\code\\.stack-work\\install\\1dd550c8\\etc"

getBinDir     = catchIO (getEnv "huffman_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "huffman_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "huffman_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "huffman_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "huffman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "huffman_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
