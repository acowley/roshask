-- Use a package's manifest.xml file to find paths to the packages on
-- which this package is dependent.
module Ros.Core.Build.DepFinder (findPackageDeps, findPackageDepNames, 
                                 findMessages, findMessage, findMessagesInPkg,
                                 findDepsWithMessages, hasMsgs) where
import Control.Applicative ((<$>))
import Control.Monad (when, filterM)
import Data.Maybe (mapMaybe, isNothing, fromJust)
import Data.List (find, findIndex, nub)
import System.Directory (doesFileExist, doesDirectoryExist, 
                         getDirectoryContents)
import System.Environment (getEnvironment)
import System.FilePath ((</>), splitSearchPath, takeExtension, 
                        dropExtension, takeFileName, splitDirectories)
import Text.XML.Light

type Package = String

-- Find the path to a package based on the given search paths.
findPackagePath :: [FilePath] -> Package -> IO (Maybe FilePath)
findPackagePath search pkg = go search
    where go [] = return Nothing
          go (p:ps) = let pkgPath = p </> pkg
                      in do e <- doesDirectoryExist pkgPath
                            if e then return (Just pkgPath) else go ps

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

-- Directory listing returning the full path to each entry excluding
-- "."  and "..".
dir :: FilePath -> IO [FilePath]
dir p = getDirectoryContents p >>= return . map (p </>) . filter notDot
    where notDot s = s /= "." && s /= ".."

-- Each given path is a possible package path root, as are all of its
-- subdirectories that are stacks (indicated by the presence of a
-- stack.xml file).
packagePaths :: FilePath -> IO [FilePath]
packagePaths path = (path :) <$> (filterM isStack =<< dir path)
    where isStack = doesFileExist . (</> "stack.xml")

-- Get every package directory on the ROS search path.
getRosPaths :: IO [FilePath]
getRosPaths = 
    do env <- getEnvironment
       let pPaths = case lookup "ROS_PACKAGE_PATH" env of
                      Just s -> s
                      Nothing -> error "ROS_PACKAGE_PATH not set in environment"
           rPath = case lookup "ROS_ROOT" env of
                     Just s -> s
                     Nothing -> error "ROS_ROOT not set in environment"
           allPaths = rPath : (rPath</>"core") : splitSearchPath pPaths
       concat <$> (mapM packagePaths =<< filterM doesDirectoryExist allPaths)

-- Packages that we will ignore for tracking down message definition
-- dependencies.
ignoredPackages :: [String]
ignoredPackages = ["genmsg_cpp", "rospack", "rosconsole", "rosbagmigration", 
                   "roscpp", "rospy", "roslisp", "std_srvs"]

-- |Find the names of the ROS packages this package depends on as
-- indicated by the manifest.xml file in this package's root
-- directory.
findPackageDepNames :: FilePath -> IO [String]
findPackageDepNames pkgRoot = 
  let man = pkgRoot </> "manifest.xml"
  in do exists <- doesFileExist man
        when (not exists)
             (error $ "Couldn't find "++man)
        txt <- readFile man
        case getPackages txt of
          Nothing -> error $ "Couldn't parse " ++ man
          Just pkgs -> return $ filter (not . (`elem` ignoredPackages)) pkgs

-- |Returns 'True' if the ROS package at the given 'FilePath' defines
-- any messages.
hasMsgs :: FilePath -> IO Bool
hasMsgs pkgPath = not. null . filter ((== ".msg") . takeExtension) <$> 
                  getDirectoryContents (pkgPath </> "msg")

-- |Find the names of the ROS packages the package at the given
-- 'FilePath' depends on as indicated by its @manifest.xml@ file. Only
-- those packages that define messages are returned.
findDepsWithMessages :: FilePath -> IO [String]
findDepsWithMessages pkgRoot = 
  do names <- findPackageDepNames pkgRoot
     searchPaths <- getRosPaths
     pkgPaths <- mapM (findPackagePath searchPaths) names
     map fst <$> filterM (maybe (return False) hasMsgs . snd) 
                         (zip names pkgPaths)

-- |Find the paths to the packages this package depends on as
-- indicated by the manifest.xml file in this package's root
-- directory.
findPackageDeps :: FilePath -> IO [FilePath]
findPackageDeps pkgRoot = 
    do pkgs <- findPackageDepNames pkgRoot
       searchPaths <- getRosPaths
       -- Add an implicit dependency on the "roslib" package.
       let pkgs' = case last (splitDirectories pkgRoot) of
                     "roslib" -> nub pkgs
                     _ -> nub $ "roslib":pkgs
       pkgPaths <- mapM (findPackagePath searchPaths) pkgs'
       case findIndex isNothing pkgPaths of
         Just i -> putStrLn ("Looking for "++show pkgs'++
                             ", dependencies of"++pkgRoot) >>
                   error ("Couldn't find path to package " ++ (pkgs' !! i))
         Nothing -> return $ map fromJust pkgPaths

-- |Return the full path to every .msg file in the given package
-- directory.
findMessages :: FilePath -> IO [FilePath]
findMessages path = aux =<< doesDirectoryExist msgPath
    where msgPath = path </> "msg"
          aux True = return . filter isMsg =<< dir msgPath
          aux False = return []
          isMsg = (==".msg") . takeExtension

-- |Find all message definition files in a ROS package. Returns the
-- 'FilePath' to the package, and the 'FilePath' to each message
-- definition in the package.
findMessagesInPkg :: String -> IO (FilePath, [FilePath])
findMessagesInPkg pkgName = do searchPaths <- getRosPaths
                               pkgPath <- maybe err id <$> 
                                          findPackagePath searchPaths pkgName
                               msgs <- findMessages pkgPath
                               return (pkgPath, msgs)
    where err = error $ "Couldn't find path to package " ++ pkgName

-- |Find the path to the message definition (.msg) file for a message
-- type with the given name. The first argument is a home package used
-- to resolve unqualified message names. The second argument is the
-- name of the message type either in the form "pkgName/typeName" or
-- "typeName". The specified package will be searched for using the
-- search paths indicated in the current environment (ROS_PACKAGE_PATH
-- and ROS_ROOT).
findMessage :: String -> String -> IO (Maybe FilePath)
findMessage msgPkg msgType = 
    do searchPaths <- getRosPaths
       -- Add an implicit dependency on the "roslib" package.
       let pkgs = [ msgPkg, "roslib" ]
       pkgPaths <- mapM (findPackagePath searchPaths) pkgs
       case findIndex isNothing pkgPaths of
         Just i -> putStrLn ("Looking for "++msgPkg++"."++msgType) >>
                   error ("Couldn't find path to package " ++ (pkgs !! i))
         Nothing -> find isMsg . concat <$> 
                    mapM (findMessages . fromJust) pkgPaths
    where isMsg = (== msgType) . dropExtension . takeFileName
