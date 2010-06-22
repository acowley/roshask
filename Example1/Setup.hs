import Control.Applicative
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Environment
import System.FilePath

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c lst = go lst 
    where go [] = [] 
          go lst' = let (h,t) = break (== c) lst' in h : go t

getMsgPaths :: FilePath -> IO [FilePath]
getMsgPaths = undefined

addRosPaths :: Args -> BuildFlags -> IO HookedBuildInfo
addRosPaths _ bf = 
    do env <- getEnvironment
       let pPaths = case lookup "ROS_PACKAGE_PATH" env of
                      Just s -> s
                      Nothing -> error "ROS_PACKAGE_PATH not set in environment"
           rPath = case lookup "ROS_ROOT" env of
                     Just s -> s
                     Nothing -> error "ROS_ROOT not set in environment"
       paths <- concat <$> mapM getMsgPaths (rPath : splitOn ':' pPaths)
       let binfo = emptyBuildInfo { hsSourceDirs = paths }
       return (Just binfo, [])

main = defaultMainWithHooks $ simpleUserHooks { preBuild = addRosPaths }
