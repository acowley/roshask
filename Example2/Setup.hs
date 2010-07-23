import Control.Applicative
import Data.List (intercalate)
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Ros.Build.DepFinder

-- Add the message directories of the package's we are dependent on to
-- GHC's path.
addRosMsgPaths :: Args -> BuildFlags -> IO HookedBuildInfo
addRosMsgPaths _ bf = 
    do dir <- getCurrentDirectory
       msgPaths <- map (</>"msg"</>"haskell") <$> findPackageDeps dir
       writeFile ".ghci" $ ":set -i"++ intercalate ":" msgPaths
       let binfo = emptyBuildInfo { hsSourceDirs = msgPaths }
       return (Nothing, [("RosHask2", binfo)])

main = defaultMainWithHooks $ simpleUserHooks { preBuild = addRosMsgPaths }
