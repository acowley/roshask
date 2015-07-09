-- |The main entry point for the roshask executable.
module Main (main) where
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath (isRelative, (</>), dropFileName,
                        takeFileName, takeExtension, dropExtension)
import Ros.Internal.DepFinder (findMessages, findPackageDeps, 
                               findPackageDepsTrans)
import Ros.Internal.PathUtil (cap, codeGenDir, pathToPkgName)

import Analysis (runAnalysis)
import Parse
import Gen
import MD5
import PkgBuilder (buildPkgMsgs, pathToRosPkg,
                   parseGenWriteMsg, parseGenWriteService)
import Unregister
import PkgInit (initPkg)

-- Generate and save Haskell code for a message or service file.
generateAndSave :: FilePath -> IO ()
generateAndSave fname = do
    destDir <- codeGenDir fname
    let hsNames = [(B.pack . cap . dropExtension . takeFileName) fname]
    runAnalysis $ case takeExtension fname of
        ".msg" -> parseGenWriteMsg     pkgHier destDir hsNames fname
        ".srv" -> parseGenWriteService pkgHier destDir hsNames fname
        _ -> error $ "Unknown file type: " ++ fname
  where
    pkgName = pathToRosPkg fname
    pkgHier = B.pack $ "Ros." ++ cap pkgName ++ "."

-- Generate Haskell code for a message type.
generate :: FilePath -> IO (B.ByteString, String)
generate fname = 
    do r <- parseMsg fname
       pkgMsgs <- map B.pack <$> findMessages dir
       case r of
         Left err -> do putStrLn $ "ERROR: " ++ err
                        exitWith (ExitFailure (-2))
         Right msg -> runAnalysis $ 
                      do hMsg <- generateMsgType pkgHier pkgMsgs msg
                         md5 <- msgMD5 msg
                         return (hMsg, md5)
    where pkgHier = B.pack $ "Ros." ++ init pkgName ++ "."
          dir = dropFileName fname
          pkgName = pathToPkgName dir

-- |Run "roshask gen" on all the .msg files in each of the given
-- package directories.
buildDepMsgs :: [FilePath] -> IO ()
buildDepMsgs = runAnalysis . mapM_ buildPkgMsgs

-- When given a relative path, prepend it with the current working
-- directory.
canonicalizeName :: FilePath -> IO FilePath
canonicalizeName fname = if isRelative fname
                         then (</> fname) <$> getCurrentDirectory
                         else return fname

help :: [String]
help = [ "Usage: roshask command [[arguments]]"
       , "Available commands:"
       , "  create pkgName [[dependencies]]  -- Create a new ROS package with "
       , "                                      roshask support"
       , ""
       , "  gen file.msg                     -- Generate Haskell message code"
       , ""
       , "  dep                              -- Build all messages this package "
       , "                                      depends on"
       , ""
       , "  dep directory                    -- Build all messages the specified "
       , "                                      package depends on" 
       , ""
       , "  md5 file.msg                     -- Generate an MD5 sum for a ROS "
       , "                                      message type"
       , ""
       , "  unregister                       -- Unregister all \"ROS-\"-prefixed"
       , "                                      packages using ghc-pkg. This is"
       , "                                      useful when you upgrade roshask"
       , "                                      and wish to remove all "
       , "                                      previously generated message"
       , "                                      libraries." ]

main :: IO ()
main = do args <- getArgs
          case args of
            ["gen",name] -> canonicalizeName name >>= generateAndSave
            ["md5",name] -> canonicalizeName name >>= 
                            generate >>= putStrLn . snd
            ("create":pkgName:deps) -> initPkg pkgName deps
            ["unregister"] -> unregisterInteractive
            ["dep"] -> do d <- getCurrentDirectory 
                          deps <- findPackageDepsTrans d
                          buildDepMsgs (deps++[d])
            ["dep",name] -> findPackageDeps name >>= (buildDepMsgs . (++[name]))
            _ -> do mapM_ putStrLn help
                    exitWith (ExitFailure (-1))
