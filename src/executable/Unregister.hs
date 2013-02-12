-- |Helper to unregister all ROS-related packages known by @ghc-pkg@.
module Unregister (unregisterInteractive) where
import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import System.Process

findROSPackages :: IO [String]
findROSPackages = mapMaybe (isROS . dropWhile isSpace) . lines
               <$> readProcess "ghc-pkg" ["list"] "" 
  where isROS s
          | "ROS-" `isPrefixOf` s = Just s
          | otherwise = Nothing

unregisterPackage :: String -> IO ()
unregisterPackage pkg = rawSystem "ghc-pkg" ["unregister", "--force", pkg] 
                        >>= aux
  where aux ExitSuccess = return ()
        aux (ExitFailure e) = putStrLn $ "ghc-pkg unregister "++pkg++
                                         " failed: exit code "++show e

unregisterInteractive :: IO ()
unregisterInteractive = 
  do pkgs <- findROSPackages
     if null pkgs
       then putStrLn "No ROS packages to remove!"
       else do putStrLn "Ready to remove the following packags:"
               mapM_ (putStrLn . ("  "++)) pkgs
               putStr "Are you sure you want to continue (y/n)? "
               hFlush stdout
               s <- map toLower <$> getLine
               if s == "n" || s == "no"
                 then putStrLn "Cancelling unregistration."
                 else mapM_ unregisterPackage pkgs >>
                      putStrLn "Unregistration complete!"