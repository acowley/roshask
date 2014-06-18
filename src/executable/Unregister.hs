-- |Helper to unregister all ROS-related packages known by @ghc-pkg@.
module Unregister (unregisterInteractive) where
import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import PkgBuilder (myReadProcess, toolPaths, ToolPaths(..))
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import System.Process

findROSPackages :: ToolPaths -> IO [String]
findROSPackages tools =
  mapMaybe (isROS . dropWhile isSpace) . lines
  <$> myReadProcess (ghcPkg tools ["list"])
  where isROS s
          | "ROS-" `isPrefixOf` s = Just s
          | otherwise = Nothing

unregisterPackage :: ToolPaths -> String -> IO ()
unregisterPackage tools pkg =
  do (_,_,_, ph) <-
       createProcess (ghcPkg tools ("unregister":pkg:(forceFlag tools)))
     waitForProcess ph >>= \ex -> case ex of
       ExitSuccess -> return ()
       ExitFailure e -> putStrLn $ "Unregistering "++pkg++" failed: "++show e

unregisterInteractive :: IO ()
unregisterInteractive = 
  do tools <- toolPaths
     pkgs <- findROSPackages tools
     if null pkgs
       then putStrLn "No ROS packages to remove!"
       else do putStrLn "Ready to remove the following packags:"
               mapM_ (putStrLn . ("  "++)) pkgs
               putStr "Are you sure you want to continue (y/n)? "
               hFlush stdout
               s <- map toLower <$> getLine
               if s == "n" || s == "no"
                 then putStrLn "Cancelling unregistration."
                 else mapM_ (unregisterPackage tools) pkgs >>
                      putStrLn "Unregistration complete!"
