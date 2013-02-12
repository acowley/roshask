-- |The main entry point for the roshask executable.
module Main (main) where
import Control.Applicative
import Control.Monad ((>=>), join)
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, 
                         getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath (replaceExtension, isRelative, (</>), dropFileName, 
                        takeFileName, dropExtension, takeExtension)
import Ros.Core.Msg.Analysis (runAnalysis)
import Ros.Core.Msg.Parse
import Ros.Core.Msg.Gen
import Ros.Core.Msg.MD5
import Ros.Core.Msg.PkgBuilder (buildPkgMsgs)
import Ros.Core.Msg.Unregister
import Ros.Core.Build.DepFinder (findMessages, findPackageDeps, 
                                 findPackageDepsTrans)
import Ros.Core.Build.Init (initPkg)
import Ros.Core.PathUtil (cap, codeGenDir, pathToPkgName)

-- Get a list of all messages defined in a directory.
-- pkgMessages :: FilePath -> IO [FilePath]
-- pkgMessages = fmap (map (cap . dropExtension) .
--                     filter ((== ".msg") . takeExtension)) .
--               getDirectoryContents
pkgMessages = findMessages

generateAndSave :: FilePath -> IO ()
generateAndSave fname = do msgType <- fst <$> generate fname
                           fname' <- hsName
                           B.writeFile fname' msgType
  where hsName = do d' <- codeGenDir fname
                    createDirectoryIfMissing True d'
                    return $ d' </> f
        f =  replaceExtension (takeFileName fname) ".hs"
        -- d' = d </> "haskell" </> "Ros" </> pkgName

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
          pkgName = pathToPkgName dir

-- |Run "roshask gen" on all the .msg files in each of the given
-- package directories.
buildDepMsgs :: [FilePath] -> IO ()
buildDepMsgs = runAnalysis . mapM_ buildPkgMsgs

canonicalizeName :: FilePath -> IO FilePath
canonicalizeName fname = if isRelative fname
                         then (</> fname) <$> getCurrentDirectory
                         else return fname

helP :: [String]
helP = [ "Usage: roshask command [[arguments]]"
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
       , "  unregister                       -- Unregister all"
       , "                                      \"ROS-\"-prefixed packages using "
       , "                                      ghc-pkg. This is useful when you "
       , "                                      upgrade roshask and wish to "
       , "                                      remove all previously generated "
       , "                                      message libraries." ]

options :: Parser (IO ())
options = subparser $
          command "gen" (info genOptions (progDesc genDesc))
       <> command "md5" (info md5Options (progDesc md5Desc))
       <> command "create" (info createOptions (progDesc createDesc))
       <> command "dep" (info depOptions depDesc)
       <> command "unregister" (info unregOptions (progDesc unregDesc))
  where genOptions = (canonicalizeName >=> generateAndSave) <$>
                     (helper <*> argument str (metavar "FILE"))
        genDesc = "Generate Haskell message code for the given file.msg"
        md5Options = (canonicalizeName >=> generate >=> putStrLn . snd) <$>
                     (helper <*> argument str (metavar "FILE"))
        md5Desc = "Generate an MD5 sum for a ROS message specification"
        createOptions = uncurry initPkg <$> 
                        (helper <*>
                        ((,) <$> argument str (metavar "PACKAGE_NAME") 
                             <*> arguments str (metavar "dependencies...")))
        createDesc = "Create a new ROS package with roshask support"
        depOptions = depAux <$> 
                     (helper <*> argument str (metavar "[PACKAGE_DIRECTORY]"))
        depAux "" = do d <- getCurrentDirectory
                       findPackageDepsTrans d >>= buildDepMsgs . (++[d])
        depAux d = findPackageDeps d >>= (buildDepMsgs . (++[d]))
        depDesc = progDesc $ unlines
                  [ "Build all messages the specified package depends on."
                  , "If no argument is given, all messages the package in" 
                  , "the current directory depends on are built." ]
        unregOptions = helper <*> pure unregisterInteractive
        unregDesc = "Unregister all \"ROS-\"-prefixed packages known to "++
                    "ghc-pkg. This is useful when you upgrade roshask, and "++
                    "wish to remove all previously generated message libraries."

main :: IO ()
main = join . execParser $ 
       info (helper <*> options) 
            (fullDesc <>
             (progDesc $ "The roshask build tool can help with "++
                         "initializating new ROS packages, and generating "++
                         "Haskell implementations of ROS message "++
                         "specifications (.msg files)."))

-- main = do args <- getArgs
--           case args of
--             ["gen",name] -> canonicalizeName name >>= generateAndSave
--             ["md5",name] -> canonicalizeName name >>= 
--                             generate >>= putStrLn . snd
--             ("create":pkgName:deps) -> initPkg pkgName deps
--             ["unregister"] -> unregisterInteractive
--             ["dep"] -> do d <- getCurrentDirectory 
--                           deps <- findPackageDepsTrans d
--                           buildDepMsgs (deps++[d])
--             ["dep",name] -> findPackageDeps name >>= (buildDepMsgs . (++[name]))
--             _ -> do mapM_ putStrLn helP
--                     exitWith (ExitFailure (-1))
