{-# LANGUAGE OverloadedStrings #-}
-- |Build all messages defined in a ROS package.
module Ros.Core.Msg.PkgBuilder where
import Control.Applicative
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Either (rights)
import Data.List (findIndex, intercalate, isSuffixOf)
import System.Directory (createDirectoryIfMissing, getDirectoryContents,
                         doesDirectoryExist, removeFile)
import System.FilePath
import System.Process (createProcess, proc, CreateProcess(..), waitForProcess)
import System.Exit (ExitCode(..))
import Ros.Core.Build.DepFinder (findMessages, findDepsWithMessages, hasMsgs)
import Ros.Core.Msg.Analysis
import Ros.Core.Msg.Gen (generateMsgType)
import Ros.Core.Msg.Parse (parseMsg)
import Data.ByteString.Char8 (ByteString)
import Paths_roshask (version)
import Data.Version (versionBranch)

-- The current version of roshask.
roshaskVersion :: B.ByteString
roshaskVersion = B.pack . intercalate "." $ map show (versionBranch version)

-- The current version of roshask with the patch level changed to an
-- asterisk. The intension is to allow a compiled roshask package to
-- work with newer patch levels of roshask itself.
roshaskMajorMinor :: B.ByteString
roshaskMajorMinor = B.pack . intercalate "." $ 
                    map show (take 2 (versionBranch version)) ++ ["*"]

-- Build all messages defined by a package.
buildPkgMsgs :: FilePath -> MsgInfo ()
buildPkgMsgs fname = do liftIO . putStrLn $ "Generating package " ++ fname
                        liftIO $ createDirectoryIfMissing True destDir
                        pkgMsgs <- liftIO $ findMessages fname
                        let pkgMsgs' = map (B.pack . cap . 
                                            dropExtension . takeFileName)
                                           pkgMsgs
                            checkErrors xs = case findIndex isLeft xs of
                                               Nothing -> rights xs
                                               Just i -> err (pkgMsgs !! i)
                            names = map ((destDir </>) .
                                        flip replaceExtension ".hs" . 
                                        takeFileName)
                                        pkgMsgs
                            gen = generateMsgType pkgHier pkgMsgs'
                        parsed <- liftIO $ checkErrors <$> mapM parseMsg pkgMsgs
                        mapM_ (\(n, m) -> gen m >>= 
                                          liftIO . B.writeFile n)
                              (zip names parsed)
                        liftIO $ do f <- hasMsgs fname
                                    when f (removeOldCabal fname >> compileMsgs)
                          
    where err pkg = error $ "Couldn't parse message " ++ pkg
          destDir = fname </> "msg" </> "haskell" </> "Ros" </> cap pkgName
          pkgName = last . splitDirectories $ fname
          pkgHier = B.pack $ "Ros." ++ cap pkgName ++ "."
          isLeft (Left _) = True
          isLeft _ = False
          compileMsgs = do cpath <- genMsgCabal fname pkgName
                           let cpath' = dropFileName cpath
                           (_,_,_,proc) <- 
                             createProcess (proc "cabal" ["install"])
                                           { cwd = Just cpath' }
                           code <- waitForProcess proc
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
  do f <- doesDirectoryExist msgPath
     when f (getDirectoryContents msgPath >>=
             mapM_ (removeFile . (msgPath </>)) . 
                   filter ((== ".cabal") . takeExtension))
  where msgPath = pkgPath </> "msg" </> "haskell"

-- Capitalize the first letter in a string.
cap :: String -> String
cap [] = []
cap (h:t) = toUpper h : t

-- Extract a Msg module name from a Path
path2MsgModule :: FilePath -> String
path2MsgModule = intercalate "." . map cap . reverse . take 3 .
                 reverse . splitDirectories . dropExtension

getHaskellMsgFiles :: FilePath -> String -> IO [FilePath]
getHaskellMsgFiles pkgPath pkgName = 
  map (dir </>) . filter ((== ".hs") . takeExtension) <$> getDirectoryContents dir
  where dir = pkgPath </> "msg" </> "haskell" </> "Ros" </> pkgName

-- Generate a .cabal file to build this ROS package's messages.
genMsgCabal :: FilePath -> String -> IO FilePath
genMsgCabal pkgPath pkgName = 
  do deps <- map (B.pack . rosPkg2CabalPkg) <$> 
             findDepsWithMessages pkgPath
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
                  , "                   vector == 0.7.*,"
                  , "                   time == 1.1.*,"
                  , B.concat [ "                   roshask == "
                             , roshaskMajorMinor
                             , if null deps then ""  else "," ]
                  , B.intercalate ",\n" $
                    map (B.append "                   ") deps
                  , "  GHC-Options:     -Odph" ]
         pkgDesc = B.concat [preamble, "\n", target]
         cabalFilePath = pkgPath</>"msg"</>"haskell"</>cabalPkg++".cabal"
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