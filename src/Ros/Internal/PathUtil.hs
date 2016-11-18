module Ros.Internal.PathUtil where
import Data.Char (toUpper)
import Data.List (tails)
import System.Directory (doesFileExist)
import System.FilePath
import Paths_roshask

-- |Ensure that the first character in a String is capitalized.
cap :: String -> String
cap [] = []
cap (x:xs) = toUpper x : xs

-- |Determine if a path is a directory containing a ROS package.
isPkg :: FilePath -> IO Bool
isPkg = doesFileExist . (</> "manifest.xml")

-- |Determine if a path is a directory containing a ROS stack.
isStack :: FilePath -> IO Bool
isStack = doesFileExist . (</> "stack.xml")

-- |Identify the name of the package defining a msg.
pathToPkgName :: FilePath -> String
pathToPkgName p
  | hasExtension p = cap . last . init . splitPath $ p
  | otherwise = cap . last . splitPath $ p

-- |Identify the name of the stack in which a msg is defined. If the
-- package definining the message does not live in a stack, the result
-- is 'Nothing'.
stackName :: FilePath -> IO (Maybe String)
stackName = go . tails . reverse . splitPath
  where go :: [[FilePath]] -> IO (Maybe String)
        go [] = return Nothing
        go [[]] = return Nothing
        go (d:ds) = do b <- isStack . joinPath . reverse $ d
                       if b then return (Just (head d)) else go ds

-- |Given a path to a msg definition file, compute a destination
-- directory for generated Haskell code. A typical path will be under,
-- @~/.cabal/share/roshask/@.
codeGenDir :: FilePath -> IO FilePath
codeGenDir f = do s <- stackName f
                  r <- getDataDir
                  let base = case s of
                               Nothing -> r
                               Just s' -> r </> s'
                  return $ base </> pkg </> "Ros" </> pkg
  where pkg = pathToPkgName f
