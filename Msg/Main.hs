module Main (main) where
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath (replaceExtension, splitFileName, splitPath, 
                        isRelative, (</>))
import Msg.Types
import Msg.Parse
import Msg.Gen
import Ros.Build.DepFinder (findPackageDeps, buildDepMsgs)

generate :: FilePath -> IO ()
generate fname = do r <- parseMsg fname
                    case r of
                      Left err -> do putStrLn $ "ERROR: " ++ err
                                     exitWith (ExitFailure (-2))
                      Right msg -> do fname' <- hsName
                                      B.writeFile fname' 
                                                  (generateMsgType pkgHier msg)
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

main = do args <- getArgs
          case args of
            ["gen",name] -> canonicalizeName name >>= generate
            ["dep"] -> getCurrentDirectory >>= findPackageDeps >>= buildDepMsgs
            ["dep",name] -> findPackageDeps name >>= buildDepMsgs
            _ -> do putStrLn "Usage: roshask command [argument]" 
                    putStrLn "Available commands:"
                    putStrLn "  gen file.msg  -- generate Haskell message code"
                    putStrLn ("  dep           -- build all messages this "++
                              "package depends on")
                    putStrLn ("  dep directory -- build all messages the "++
                              "specified package depends on")
                    exitWith (ExitFailure (-1))
