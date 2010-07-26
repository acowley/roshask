{-# LANGUAGE TupleSections #-}
module Ros.Build.SetupUtil (addRosMsgPaths, Buildable(..)) where
import Control.Applicative
import Data.List (intercalate)
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription hiding (Library)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Ros.Build.DepFinder

data Buildable = Library 
               | LibraryAndExecutables [String] 
               | Executables [String]

-- Add the message directories of the package's we are dependent on to
-- GHC's path.
addRosMsgPaths :: Buildable -> Args -> BuildFlags -> IO HookedBuildInfo
addRosMsgPaths targets _ _ = 
    do dir <- getCurrentDirectory
       msgPaths <- map (</>"msg"</>"haskell") <$> findPackageDeps dir
       writeFile ".ghci" $ ":set -i"++ intercalate ":" msgPaths
       let binfo = emptyBuildInfo { hsSourceDirs = msgPaths }
       case targets of
         Library -> return (Just binfo, [])
         LibraryAndExecutables exes -> return (Just binfo, map (,binfo) exes)
         Executables exes -> return (Nothing, map (,binfo) exes)
