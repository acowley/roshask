module Main (main) where
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath (replaceExtension, splitFileName, splitPath, (</>))
import Msg.Types
import Msg.Parse
import Msg.Gen

generate :: FilePath -> IO ()
generate fname = do r <- parseMsg fname
                    case r of
                      Left err -> do putStrLn $ "ERROR: " ++ err
                                     exitWith (ExitFailure (-2))
                      Right msg -> do fname' <- hsName
                                      B.writeFile fname' (generateMsgType msg)
    where hsName = let (d,f) = splitFileName $ replaceExtension fname ".hs"
                       cap s = toUpper (head s) : tail s
                       pkgName = cap . last . init . splitPath $ d
                       d' = d </> "haskell" </> "Ros" </> pkgName
                   in do createDirectoryIfMissing True d'
                         return $ d' </> f

main = do args <- getArgs
          case args of
            [name] -> generate name
            _ -> do putStrLn "Expected 1 argument: path to .msg file"
                    exitWith (ExitFailure (-1))