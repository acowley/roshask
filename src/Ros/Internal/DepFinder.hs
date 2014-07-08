-- Use a package's manifest.xml file to find paths to the packages on
-- which this package is dependent.
module Ros.Internal.DepFinder (findPackageDeps, findPackageDepNames, 
                               findPackageDepsTrans,
                               findMessages, findMessage, findMessagesInPkg,
                               findDepsWithMessages, hasMsgs
                              ) where
import Control.Applicative ((<$>))
import Control.Monad (when, filterM)
import Data.Maybe (mapMaybe, isNothing, fromJust)
import Data.List (find, findIndex, nub)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>), splitSearchPath, dropExtension, 
                        takeFileName, splitPath)
import System.FilePath.Find hiding (find)
import qualified System.FilePath.Find as F
import Text.XML.Light

type Package = String

-- Find the path to a package based on the given search paths.
findPackagePath :: [FilePath] -> Package -> Maybe FilePath
findPackagePath search pkg = find ((== pkg) . last . splitPath) search

-- Get the packages listed as dependencies in an XML manifest.  NOTE:
-- In version 1.3.7, the xml package gained the ability to work with
-- ByteStrings via the XmlSource typeclass. Consider that upgrade if
-- performance is causing trouble.
getPackages :: String -> Maybe [Package]
getPackages = (map attrVal . 
               mapMaybe (find ((==pkg).attrKey) . elAttribs) . 
               findChildren dep <$>) . 
              parseXMLDoc
    where pkg = QName "package" Nothing Nothing
          dep = QName "depend" Nothing Nothing

-- The catkin build system uses a @package.xml@ file that is somewhat
-- different from the older @manifest.xml@.
catkinBuildDeps :: String -> Maybe [Package]
catkinBuildDeps = fmap (map strContent . findChildren dep) . parseXMLDoc
  where dep = QName "build_depend" Nothing Nothing

-- The given path is a possible package path root, as are all of its
-- subdirectories that are stacks (indicated by the presence of a
-- stack.xml file). Returns a list of package directories.
packagePaths :: FilePath -> IO [FilePath]
packagePaths = F.find always $
               contains "manifest.xml" ||? contains "package.xml"

-- Get every package directory on the ROS search path.
getRosPaths :: IO [FilePath]
getRosPaths = 
    do env <- getEnvironment
       let pPaths = case lookup "ROS_PACKAGE_PATH" env of
                      Just s -> s
                      Nothing -> error "ROS_PACKAGE_PATH not set in environment"
           -- rPath = case lookup "ROS_ROOT" env of
           --           Just s -> s
           --           Nothing -> error "ROS_ROOT not set in environment"
           allPaths = splitSearchPath pPaths
       concat <$> (mapM packagePaths =<< filterM doesDirectoryExist allPaths)

-- Packages that we will ignore for tracking down message definition
-- dependencies.
ignoredPackages :: [String]
ignoredPackages = ["genmsg_cpp", "rospack", "rosconsole", "rosbagmigration", 
                   "roscpp", "rospy", "roslisp", "std_srvs", "roslib", "boost"]

-- |Find the names of the ROS packages this package depends on as
-- indicated by the manifest.xml file in this package's root
-- directory.
findPackageDepNames :: FilePath -> IO [String]
findPackageDepNames pkgRoot = 
  let manifest = pkgRoot </> "manifest.xml"
      pkg = pkgRoot </> "package.xml"
  in do exists <- doesFileExist manifest
        existsCatkin <- doesFileExist pkg
        when (not $ exists || existsCatkin)
             (error $ "Couldn't find "++manifest++" or "++pkg)
        pkgs <- if exists
                then getPackages <$> readFile manifest
                else catkinBuildDeps <$> readFile pkg
        case pkgs of
          Nothing -> error $ "Couldn't parse package file for " ++ pkgRoot
          Just ps -> return . nub $ filter (not . (`elem` ignoredPackages)) ps

-- |Returns 'True' if the ROS package at the given 'FilePath' defines
-- any messages.
hasMsgs :: FilePath -> IO Bool
hasMsgs = fmap (not . null) . F.find (depth <? 2) (extension ==? ".msg")

{-
-- |Returns 'True' if the ROS package at the given 'FilePath' is a
-- roshask package (determined by the presence of a @.cabal@ file in
-- the package's root directory).
isRoshask :: FilePath -> IO Bool
isRoshask pkgPath = not . null . filter ((== ".cabal") . takeExtension) <$> 
                    getDirectoryContents pkgPath
-}

-- |Find the names of the ROS packages the package at the given
-- 'FilePath' depends on as indicated by its @manifest.xml@ file. Only
-- those packages that define messages are returned.
findDepsWithMessages :: FilePath -> IO [String]
findDepsWithMessages pkgRoot = 
  do names <- findPackageDepNames pkgRoot
     searchPaths <- getRosPaths
     filterM (maybe (return False) hasMsgs . findPackagePath searchPaths) names

-- |Find the paths to the packages this package depends on as
-- indicated by the manifest.xml file in this package's root
-- directory.
findPackageDeps :: FilePath -> IO [FilePath]
findPackageDeps pkgRoot = 
    do pkgs <- findPackageDepNames pkgRoot
       searchPaths <- getRosPaths
       let pkgPaths = map (findPackagePath searchPaths) pkgs
       case findIndex isNothing pkgPaths of
         Just i -> putStrLn ("Looking for "++show pkgs++
                             ", dependencies of"++pkgRoot) >>
                   error ("Couldn't find path to package (1) " ++ (pkgs !! i))
         Nothing -> return $ map fromJust pkgPaths

-- |Transitive closure of 'findPackageDeps'. Find the paths to the
-- packages this package depends on as indicated by the manifest.xml
-- file in this package's root directories and the manifests in the
-- root directories of the dependencies, and so on.
findPackageDepsTrans :: FilePath -> IO [FilePath]
findPackageDepsTrans pkgRoot =
  do searchPaths <- getRosPaths
     let getDeps pkg = 
           do pkgDeps <- findPackageDepNames pkg
              let pkgPaths = map (findPackagePath searchPaths) pkgDeps
              case findIndex isNothing pkgPaths of
                Just i -> putStrLn ("Looking for "++show pkgDeps++
                                    ", dependencies of "++pkgRoot) >>
                          error ("Couldn't find path to package (2) " ++ 
                                 (pkgDeps !! i))
                Nothing -> return $ map fromJust pkgPaths
         recurse p = do deps <- getDeps p
                        nub . (++[p]) . concat <$> mapM recurse deps
     init <$> recurse pkgRoot
     
-- |Return the full path to every .msg file in the given package
-- directory.
findMessages :: FilePath -> IO [FilePath]
findMessages pkgRoot = 
  do e <- doesDirectoryExist dir
     if e then F.find (depth <? 1) (extension ==? ".msg") dir else return []
  where dir = pkgRoot </> "msg"

-- |Find all message definition files in a ROS package. Returns the
-- 'FilePath' to the package, and the 'FilePath' to each message
-- definition in the package.
findMessagesInPkg :: String -> IO (FilePath, [FilePath])
findMessagesInPkg pkgName = do searchPaths <- getRosPaths
                               let pkgPath = maybe err id $ 
                                             findPackagePath searchPaths pkgName
                               msgs <- findMessages pkgPath
                               return (pkgPath, msgs)
    where err = error $ "Couldn't find path to package (3) " ++ pkgName

-- |Find the path to the message definition (.msg) file for a message
-- type with the given name. The first argument is a home package used
-- to resolve unqualified message names. The second argument is the
-- name of the message type either in the form "pkgName/typeName" or
-- "typeName". The specified package will be searched for using the
-- search paths indicated in the current environment (ROS_PACKAGE_PATH
-- and ROS_ROOT).
findMessage :: String -> String -> IO (Maybe FilePath)
findMessage pkg msgType = 
    do searchPaths <- getRosPaths
       let pkgPath = findPackagePath searchPaths pkg
       case pkgPath of
         Just p -> find isMsg <$> findMessages p
         Nothing -> putStrLn ("Looking for "++pkg++"."++msgType) >>
                    error ("Couldn't find path to package " ++ pkg)
    where isMsg = (== msgType) . dropExtension . takeFileName
