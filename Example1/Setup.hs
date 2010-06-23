import Control.Applicative
import Control.Monad
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Directory
import System.Environment
import System.FilePath

-- The std_msgs directory relative to the ROS root path.
getRootMsgs :: FilePath -> FilePath
getRootMsgs = joinPath . flip (++) ["std_msgs", "msg", "haskell"] . splitPath

-- Directory listing returning full path to each entry excluding "."
-- and "..".
dir :: FilePath -> IO [FilePath]
dir p = getDirectoryContents p >>= return . map (p </>) . filter notDot
    where notDot s = s /= "." && s /= ".."

-- Stacks may contain multiple packages. A directory is a stack if it
-- contains a stack.xml file, otherwise it is a package root.
packagePaths :: FilePath -> IO [FilePath]
packagePaths path = aux =<< doesFileExist (path </> "stack.xml")
    where aux True = dir path >>= filterM doesDirectoryExist
          aux False = return [path]

-- Identifying paths to msg types involves checking in
-- path/*/msg/haskell/ for packages, and path/*/*/msg/haskell when the
-- first directory below path contains a stack.xml file.
getMsgPaths :: FilePath -> IO [FilePath]
getMsgPaths = dir >=> mapM packagePaths >=> 
              filterM doesDirectoryExist . map hsDir . concat
    where hsDir = joinPath . flip (:) ["msg", "haskell"]

addRosPaths :: Args -> BuildFlags -> IO HookedBuildInfo
addRosPaths _ bf = 
    do env <- getEnvironment
       let pPaths = case lookup "ROS_PACKAGE_PATH" env of
                      Just s -> s
                      Nothing -> error "ROS_PACKAGE_PATH not set in environment"
           rPath = case lookup "ROS_ROOT" env of
                     Just s -> s
                     Nothing -> error "ROS_ROOT not set in environment"
       paths <- (getRootMsgs rPath :)  . concat <$> 
                mapM getMsgPaths (splitSearchPath pPaths)
       let binfo = emptyBuildInfo { hsSourceDirs = paths }
       return (Just binfo, [])

main = defaultMainWithHooks $ simpleUserHooks { preBuild = addRosPaths }
-- main = putStrLn . show . hsSourceDirs . fromJust . fst =<< 
--        addRosPaths undefined undefined
