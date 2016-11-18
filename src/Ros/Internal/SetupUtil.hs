{-# LANGUAGE CPP, TupleSections #-}
-- |Integration with the Cabal build system.
module Ros.Internal.SetupUtil (rosBuild, rosConf) where
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.List (intercalate)
import Distribution.Simple
import Distribution.Simple.InstallDirs
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription hiding (Library)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Ros.Internal.DepFinder

data Buildable = LibraryAndExecutables [String] 
               | Executables [String]

-- Add the message directories of the package's we are dependent on to
-- GHC's path.
addRosMsgPaths :: Buildable -> IO HookedBuildInfo
addRosMsgPaths targets = 
    do dir <- getCurrentDirectory
       msgPaths <- map (</>"msg"</>"haskell") <$> findPackageDeps dir
       writeFile ".ghci" $ ":set -i"++ intercalate ":" msgPaths
       let binfo = emptyBuildInfo { hsSourceDirs = msgPaths }
       case targets of
         LibraryAndExecutables exes -> return (Just binfo, map (,binfo) exes)
         Executables exes -> return (Nothing, map (,binfo) exes)

-- |The @buildHook@ override integrates a new 'HookedBuildInfo' with
-- the 'PackageDescription' in order to include additional directories
-- (e.g. those with message type definitions) in the 'hsSourceDirs'
-- field.
rosBuild :: PackageDescription -> LocalBuildInfo -> UserHooks -> 
            BuildFlags -> IO ()
rosBuild pkg lbi uh bfs = do binfo <- addRosMsgPaths targets
                             let pkg' = updatePackageDescription binfo pkg
                             (buildHook simpleUserHooks) pkg' lbi uh bfs
    where exeTargets = map exeName $ executables pkg
          targets = case library pkg of
                      Nothing -> Executables exeTargets
                      Just _ -> LibraryAndExecutables exeTargets

-- The @confHook@ override takes over @cabal install@'s @--bindir@ and
-- @--libdir@ options to force binary outputs into the @bin@ and @lib@
-- subdirectories of the package directory.

-- |The @confHook@ override takes over @cabal install@'s @--bindir@
-- option to force binary outputs into the @bin@ subdirectory of the
-- package directory.
rosConf :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> 
           IO LocalBuildInfo
rosConf x cf = do lbi <- (confHook simpleUserHooks) x cf
                  let oldDirs = installDirTemplates lbi
                      customDirs = oldDirs { bindir = toPathTemplate "bin" }
                                           -- , libdir = toPathTemplate "lib" }
                      lbi' = lbi { installDirTemplates = customDirs }
                  return lbi'

