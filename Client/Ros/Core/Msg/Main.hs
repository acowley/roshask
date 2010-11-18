module Main (main) where
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, 
                         getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath (replaceExtension, splitFileName, splitPath, isRelative,
                        (</>), dropFileName, dropExtension, takeExtension)
import Ros.Core.Msg.Analysis (runAnalysis)
import Ros.Core.Msg.Parse
import Ros.Core.Msg.Gen
import Ros.Core.Msg.MD5
import Ros.Core.Msg.PkgBuilder (buildPkgMsgs)
import Ros.Core.Build.DepFinder (findPackageDeps)
import Ros.Core.Build.Init (initPkg)

-- Ensure that the first character in a String is capitalized.
cap :: String -> String
cap s = toUpper (head s) : tail s

-- Get a list of all messages defined in a directory.
pkgMessages :: FilePath -> IO [FilePath]
pkgMessages = fmap (map (cap . dropExtension) .
                    filter ((== ".msg") . takeExtension)) .
              getDirectoryContents

generateAndSave :: FilePath -> IO ()
generateAndSave fname = do msgType <- fst <$> generate fname
                           fname' <- hsName
                           B.writeFile fname' msgType
  where hsName = do createDirectoryIfMissing True d'
                    return $ d' </> f
        (d,f) = splitFileName $ replaceExtension fname ".hs"
        pkgName = cap . last . init . splitPath $ d
        d' = d </> "haskell" </> "Ros" </> pkgName

generate :: FilePath -> IO (B.ByteString, String)
generate fname = 
    do r <- parseMsg fname
       pkgMsgs <- map B.pack <$> pkgMessages dir
       case r of
         Left err -> do putStrLn $ "ERROR: " ++ err
                        exitWith (ExitFailure (-2))
         Right msg -> runAnalysis $ 
                      do hMsg <- generateMsgType pkgHier pkgMsgs msg
                         md5 <- msgMD5 msg
                         return (hMsg, md5)
    where pkgHier = B.pack $ "Ros." ++ init pkgName ++ "."
          dir = dropFileName fname
          pkgName = cap . last . init . splitPath $ dir

-- |Run "roshask gen" on all the .msg files in each of the given
-- package directories.
buildDepMsgs :: [FilePath] -> IO ()
buildDepMsgs = runAnalysis . mapM_ buildPkgMsgs

canonicalizeName :: FilePath -> IO FilePath
canonicalizeName fname = if isRelative fname
                         then (</> fname) <$> getCurrentDirectory
                         else return fname

help :: [String]
help = [ "Usage: roshask command [[arguments]]"
       , "Available commands:"
       , "  create pkgName [[dependencies]]  -- Create a new ROS package with "
       , "                                      roskell support"
       , ""
       , "  gen file.msg                     -- Generate Haskell message code"
       , ""
       , "  dep                              -- Build all messages this package "
       , "                                      depends on"
       , ""
       , "  dep directory                    -- Build all messages the specified "
       , "                                      package depends on" ]

main :: IO ()
main = do args <- getArgs
          case args of
            ["gen",name] -> canonicalizeName name >>= generateAndSave
            ["md5",name] -> canonicalizeName name >>= 
                            generate >>= putStrLn . snd
            ("create":pkgName:deps) -> initPkg pkgName deps
            ["dep"] -> getCurrentDirectory >>= findPackageDeps >>= buildDepMsgs
            ["dep",name] -> findPackageDeps name >>= buildDepMsgs
            _ -> do mapM_ putStrLn help
                    exitWith (ExitFailure (-1))
