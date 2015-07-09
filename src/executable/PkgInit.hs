{-# LANGUAGE OverloadedStrings #-}
-- |Initialize a ROS Package with roshask support.
module PkgInit (initPkg) where
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Version (versionBranch)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.Process (system)

import Paths_roshask (version)
import PkgBuilder (rosPkg2CabalPkg, roshaskMajorMinor)

-- |Initialize a package with the given name in the eponymous
-- directory with the given ROS package dependencies.
initPkg :: String -> [String] -> IO ()
initPkg pkgName deps = do _ <- system pkgCmd
                          prepCMakeLists pkgName
                          prepCabal pkgName msgDeps
                          prepSetup pkgName
                          createDirectory (pkgName</>"src")
                          prepMain pkgName
                          prepLib pkgName
                          putStrLn (fyi pkgName)
    where pkgCmd = intercalate " " ("roscreate-pkg":pkgName:deps)
          msgDeps = B.intercalate ",\n" $ map (addSpaces . mkDep) deps
          addSpaces = B.append $ B.replicate 19 ' '
          mkDep = pack . rosPkg2CabalPkg

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

-- | New packages are constrained to the same major version of roshask
-- that was used to create them.
roshaskVersionBound :: ByteString
roshaskVersionBound = B.pack . ("roshask == "++)
                      . intercalate "."
                      . (++["*"])
                      . map show
                      $ take 2 (versionBranch version  ++ [0..])

-- Generate a .cabal file and a Setup.hs that will perform the
-- necessary dependency tracking and code generation.
prepCabal :: String -> ByteString -> IO ()
prepCabal pkgName rosDeps = B.writeFile (pkgName</>(pkgName++".cabal")) $
                            B.concat [preamble,"\n",target,"\n\n",lib,"\n"]
    where preamble = format [ ("Name", B.append "ROS-" $ B.pack pkgName)
                            , ("Version","0.0")
                            , ("Synopsis","I am code")
                            , ("Cabal-version",">=1.6")
                            , ("Category","Robotics")
                            , ("Build-type","Custom") ]
          target = B.intercalate "\n" $
                   [ B.concat ["Executable ", pack pkgName]
                   , "  Build-Depends:   base >= 4.2 && < 6,"
                   , "                   vector > 0.7,"
                   , "                   time >= 1.1,"
                   , "                   lens-family-core >= 1.2,"
                   , "                   lens-family-th >= 0.4.1,"
                   , "                   ROS-rosgraph-msgs,"
                   , B.concat [ "                   roshask == "
                              , roshaskMajorMinor
                              , moreDeps ]
                   , rosDeps
                   , "  GHC-Options:     -O2"
                   , "  Hs-Source-Dirs:  src"
                   , "  Main-Is:         Main.hs"
                   , B.append "  Other-Modules:   " pkgName' ]
          lib = B.intercalate "\n" $
                [ "Library"
                , "  Build-Depends:   base >= 4.2 && < 6,"
                , "                   vector > 0.7,"
                , "                   time >= 1.1,"
                , B.concat [
                  "                   ", roshaskVersionBound, moreDeps ]
                , rosDeps
                , "  GHC-Options:     -O2"
                , B.concat ["  Exposed-Modules: ", pkgName']
                , "  Hs-Source-Dirs:  src" ]  
          pkgName' = pack $ toUpper (head pkgName) : tail pkgName
          moreDeps = if B.null rosDeps then "" else ","

-- Format key-value pairs for a .cabal file
format :: [(ByteString,ByteString)] -> ByteString
format fields = B.concat $ map indent fields
    where indent (k,v) = let spaces = flip B.replicate ' ' $
                                      19 - B.length k - 1
                         in B.concat [k,":",spaces,v,"\n"]

-- Generate a Setup.hs file for use by Cabal.
prepSetup :: String -> IO ()
prepSetup pkgName = B.writeFile (pkgName</>"Setup.hs") $
                    B.concat [ "import Distribution.Simple\n"
                             , "import Ros.Internal.SetupUtil\n\n"
                             , "main = defaultMainWithHooks $\n"
                             , "       simpleUserHooks { confHook = "
                             , "rosConf }\n" ]

prepMain :: String -> IO ()
prepMain pkgName = writeFile (pkgName</>"src"</>"Main.hs") $
                   "module Main (main) where\n\
                   \import Ros.Node\n\
                   \import "++pkgName'++"\n\
                   \\n\
                   \main = runNode \""++pkgName++"\" "++nodeName++"\n"
    where pkgName' = toUpper (head pkgName) : tail pkgName
          nodeName = toLower (head pkgName) : tail pkgName

prepLib :: String -> IO ()
prepLib pkgName = writeFile (pkgName</>"src"</>pkgName'++".hs") . concat $
                  [ "module "++pkgName'++" ("++nodeName++") where\n"
                  , "import Ros.Node\n\n"
                  , nodeName++" :: Node ()\n"
                  , nodeName++" = return ()\n"]
  where pkgName' = toUpper (head pkgName) : tail pkgName
        nodeName = toLower (head pkgName) : tail pkgName
