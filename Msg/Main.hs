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
import Msg.Types
import Msg.Parse
import Msg.Gen
import Ros.Build.DepFinder (findPackageDeps, buildDepMsgs)
import Ros.Build.Init (initPkg)

generate :: FilePath -> IO ()
generate fname = 
    do r <- parseMsg fname
       pkgMsgs <- map (cap . dropExtension) . 
                  filter ((==".msg") . takeExtension) <$>
                  getDirectoryContents (dropFileName fname)
       case r of
         Left err -> do putStrLn $ "ERROR: " ++ err
                        exitWith (ExitFailure (-2))
         Right msg -> do fname' <- hsName
                         B.writeFile fname' $
                           generateMsgType pkgHier (map B.pack pkgMsgs) msg
    where hsName = do createDirectoryIfMissing True d'
                      return $ d' </> f
          (d,f) = splitFileName $ replaceExtension fname ".hs"
          cap s = toUpper (head s) : tail s
          pkgName = cap . last . init . splitPath $ d
          pkgHier = B.pack $ "Ros." ++ init pkgName ++ "."
          d' = d </> "haskell" </> "Ros" </> pkgName

canonicalizeName :: FilePath -> IO FilePath
canonicalizeName fname = if isRelative fname
                         then (</> fname) <$> getCurrentDirectory
                         else return fname

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


main = do args <- getArgs
          case args of
            ["gen",name] -> canonicalizeName name >>= generate
            ("create":pkgName:deps) -> initPkg pkgName deps
            ["dep"] -> getCurrentDirectory >>= findPackageDeps >>= buildDepMsgs
            ["dep",name] -> findPackageDeps name >>= buildDepMsgs
            _ -> do mapM_ putStrLn help
                    exitWith (ExitFailure (-1))
