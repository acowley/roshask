{-# LANGUAGE OverloadedStrings #-}
-- |Build all messages defined in a ROS package.
module PkgBuilder where
import Control.Applicative
import Control.Monad (when)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.Either (rights)
import Data.List (findIndex, intercalate, isSuffixOf, isPrefixOf, nub)
import Data.Version (versionBranch)
import System.Directory (createDirectoryIfMissing, getDirectoryContents,
                         doesDirectoryExist, removeFile)
import System.FilePath
import System.Process (createProcess, proc, CreateProcess(..), waitForProcess,
                       readProcess)
import System.Exit (ExitCode(..))

import Ros.Internal.DepFinder (findMessages, findDepsWithMessages, hasMsgs)
import Ros.Internal.PathUtil (codeGenDir, cap)
import Paths_roshask (version)

import Analysis
import Gen (generateMsgType)
import Parse (parseMsg)

-- The current version of roshask. We tag generated message packages
-- with the same version.
roshaskVersion :: B.ByteString
roshaskVersion = B.pack . intercalate "." $ map show (versionBranch version)

-- The current version of roshask with the patch level changed to an
-- asterisk. The intension is to allow a compiled roshask package to
-- work with newer patch levels of roshask itself.
roshaskMajorMinor :: B.ByteString
roshaskMajorMinor = B.pack . intercalate "." $ 
                    map show (take 2 (versionBranch version)) ++ ["*"]

pathToRosPkg :: FilePath -> FilePath
pathToRosPkg = last . splitDirectories

-- Determine if a roshask package is already registered with ghc-pkg
-- for the given ROS package.
packageRegistered :: FilePath -> IO Bool
packageRegistered pkg = any (isPrefixOf cabalPkg . dropWhile isSpace) . lines 
                     <$> readProcess "ghc-pkg" ["list", cabalPkg] ""
  where cabalPkg = (rosPkg2CabalPkg $ pathToRosPkg pkg) ++ 
                   "-" ++ B.unpack roshaskVersion

-- | Build all messages defined by a package unless that package is
-- already registered with ghc-pkg.
buildPkgMsgs :: FilePath -> MsgInfo ()
buildPkgMsgs fname = do r <- liftIO $packageRegistered fname
                        if r
                          then liftIO . putStrLn $ 
                               "Using existing " ++ pathToRosPkg fname
                          else buildNewPkgMsgs fname

-- | Generate Haskell implementations of all message definitions in
-- the given package.
buildNewPkgMsgs :: FilePath -> MsgInfo ()
buildNewPkgMsgs fname = 
  do liftIO . putStrLn $ "Generating package " ++ fname
     destDir <- liftIO $ codeGenDir fname
     liftIO $ createDirectoryIfMissing True destDir
     pkgMsgs <- liftIO $ findMessages fname
     let pkgMsgs' = map (B.pack . cap . dropExtension . takeFileName) pkgMsgs
         checkErrors xs = case findIndex isLeft xs of
                            Nothing -> rights xs
                            Just i -> err (pkgMsgs !! i)
         names = map ((destDir </>)
                      . flip replaceExtension ".hs"
                      . takeFileName)
                     pkgMsgs
         gen = generateMsgType pkgHier pkgMsgs'
     parsed <- liftIO $ checkErrors <$> mapM parseMsg pkgMsgs
     mapM_ (\(n, m) -> gen m >>= liftIO . B.writeFile n) (zip names parsed)
     liftIO $ do f <- hasMsgs fname
                 when f (removeOldCabal fname >> compileMsgs)
    where err pkg = error $ "Couldn't parse message " ++ pkg
          pkgName = pathToRosPkg fname
          pkgHier = B.pack $ "Ros." ++ cap pkgName ++ "."
          isLeft (Left _) = True
          isLeft _ = False
          compileMsgs = do cpath <- genMsgCabal fname pkgName
                           let cpath' = dropFileName cpath
                           (_,_,_,procH) <- 
                             createProcess (proc "cabal" ["install"])
                                           { cwd = Just cpath' }
                           code <- waitForProcess procH
                           when (code /= ExitSuccess)
                                (error $ "Building messages for "++
                                         pkgName++" failed")

-- |Convert a ROS package name to the corresponding Cabal package name
-- defining the ROS package's msg types.
rosPkg2CabalPkg :: String -> String
rosPkg2CabalPkg = ("ROS-"++) . addSuffix . map sanitize
  where sanitize '_' = '-'
        sanitize c   = c
        addSuffix n
          | "msgs" `isSuffixOf` n = n
          | otherwise = n ++ "-msgs"

removeOldCabal :: FilePath -> IO ()
removeOldCabal pkgPath = 
  do msgPath <- codeGenDir pkgPath
     f <- doesDirectoryExist msgPath
     when f (getDirectoryContents msgPath >>=
             mapM_ (removeFile . (msgPath </>)) . 
                   filter ((== ".cabal") . takeExtension))

-- Extract a Msg module name from a Path
path2MsgModule :: FilePath -> String
path2MsgModule = intercalate "." . map cap . reverse . take 3 .
                 reverse . splitDirectories . dropExtension

getHaskellMsgFiles :: FilePath -> String -> IO [FilePath]
getHaskellMsgFiles pkgPath _pkgName = do
  d <- codeGenDir pkgPath
  map (d </>) . filter ((== ".hs") . takeExtension) <$> getDirectoryContents d

-- Generate a .cabal file to build this ROS package's messages.
genMsgCabal :: FilePath -> String -> IO FilePath
genMsgCabal pkgPath pkgName = 
  do deps' <- map (B.pack . rosPkg2CabalPkg) <$> 
              findDepsWithMessages pkgPath
     cabalFilePath <- (</>cabalPkg++".cabal") . 
                      joinPath . init . init . splitPath <$> 
                      codeGenDir pkgPath
     let deps
           | pkgName == "std_msgs" = deps'
           | otherwise = nub ("ROS-std-msgs":deps')
     msgFiles <- getHaskellMsgFiles pkgPath pkgName
     let msgModules = map (B.pack . path2MsgModule) msgFiles
         target = B.intercalate "\n" $
                  [ "Library"
                  , B.append "  Exposed-Modules: " 
                             (if (not (null msgModules))
                              then B.concat [ head msgModules
                                            , "\n" 
                                            , B.intercalate "\n" 
                                                (map indent (tail msgModules)) ]
                              else "")
                  , ""
                  , "  Build-Depends:   base >= 4.2 && < 6,"
                  , "                   vector > 0.7,"
                  , "                   time >= 1.1,"
                  , B.concat [ "                   roshask == "
                             , roshaskMajorMinor
                             , if null deps then ""  else "," ]
                  , B.intercalate ",\n" $
                    map (B.append "                   ") deps
                  , "  GHC-Options:     -O2" ]
         pkgDesc = B.concat [preamble, "\n", target]
         --cabalFilePath = pkgPath</>"msg"</>"haskell"</>cabalPkg++".cabal"
     B.writeFile cabalFilePath pkgDesc
     return cabalFilePath
  where cabalPkg = rosPkg2CabalPkg pkgName
        preamble = format [ ("Name", B.pack cabalPkg)
                          , ("Version", roshaskVersion)
                          , ("Synopsis", B.append "ROS Messages from " 
                                                  (B.pack pkgName))
                          , ("Cabal-version", ">=1.6")
                          , ("Category", "Robotics")
                          , ("Build-type", "Simple") ]
        indent = let spaces = B.replicate 19 ' ' in B.append spaces

format :: [(ByteString, ByteString)] -> ByteString
format fields = B.concat $ map indent fields
  where indent (k,v) = let spaces = flip B.replicate ' ' $ 
                                    21 - B.length k - 1
                       in B.concat [k,":",spaces,v,"\n"]
