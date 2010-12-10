{-# LANGUAGE OverloadedStrings #-}
-- |Initialize a ROS Package with roshask support.
module Ros.Core.Build.Init (initPkg) where
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.List (intercalate)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.Process (system)

-- |Initialize a package with the given name in the eponymous
-- directory with the given ROS package dependencies.
initPkg :: String -> [String] -> IO ()
initPkg pkgName deps = do _ <- system pkgCmd
                          prepCMakeLists pkgName
                          prepCabal pkgName
                          prepSetup pkgName
                          createDirectory (pkgName</>"src")
                          prepMain pkgName
                          putStrLn (fyi pkgName)
                          
    where pkgCmd = intercalate " " ("roscreate-pkg":pkgName:deps)

fyi :: String -> String
fyi pkgName = "Created an empty roshask package.\n" ++
              "Please edit "++
              show (pkgName</>pkgName++".cabal") ++
              " to specify what should be built."

-- Add an entry to the package's CMakeLists.txt file to invoke
-- roshask.
prepCMakeLists :: String -> IO ()
prepCMakeLists pkgName = B.appendFile (pkgName</>"CMakeLists.txt") cmd
    where cmd = "\nadd_custom_target(roshask ALL roshask dep ${PROJECT_SOURCE_DIR} COMMAND cd ${PROJECT_SOURCE_DIR} && cabal install)\n"

-- Generate a .cabal file and a Setup.hs that will perform the
-- necessary dependency tracking and code generation.
prepCabal :: String -> IO ()
prepCabal pkgName = B.writeFile (pkgName</>(pkgName++".cabal")) $
                    B.concat [preamble,"\n",target]
    where preamble = format [ ("Name", B.append "ROS-" $ B.pack pkgName)
                            , ("Version","0.0")
                            , ("Synopsis","I am code")
                            , ("Cabal-version",">=1.6")
                            , ("Category","Robotics")
                            , ("Build-type","Custom") ]
          target = B.intercalate "\n" $
                   [ B.concat ["Executable ", pack pkgName]
                   , "  Build-Depends:  base >= 4.2 && < 5,"
                   , "                  vector == 0.7.*,"
                   , "                  time == 1.1.*,"
                   , "                  roshask == 0.1.*"
                   , B.concat ["  GHC-Options:    -Odph -main-is ", 
                               pack pkgName']
                   , B.concat ["  Main-Is:        ", pack pkgName', ".hs"]
                   , "  Hs-Source-Dirs: src" ]  
          pkgName' = toUpper (head pkgName) : tail pkgName

-- Format key-value pairs for a .cabal file
format :: [(ByteString,ByteString)] -> ByteString
format fields = B.concat $ map indent fields
    where indent (k,v) = let spaces = flip B.replicate ' ' $
                                      21 - B.length k - 1
                         in B.concat [k,":",spaces,v,"\n"]

-- Generate a Setup.hs file for use by Cabal.
prepSetup :: String -> IO ()
prepSetup pkgName = B.writeFile (pkgName</>"Setup.hs") $
                    B.concat [ "import Distribution.Simple\n"
                             , "import Ros.Core.Build.SetupUtil\n\n"
                             , "main = defaultMainWithHooks $\n"
                             , "       simpleUserHooks { confHook = "
                             , "rosConf, buildHook = rosBuild }\n" ]

prepMain :: String -> IO ()
prepMain pkgName = writeFile (pkgName</>"src"</>pkgName'++".hs") $
                   "module "++pkgName'++" (main) where\n\
                   \import Ros.Node\n\
                   \\n\
                   \main = putStrLn \"Hello\"\n"
    where pkgName' = toUpper (head pkgName) : tail pkgName
                       
