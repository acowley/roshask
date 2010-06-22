module Main (main) where
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath (replaceExtension)
import Msg.Types
import Msg.Parse
import Msg.Gen

generate :: FilePath -> IO ()
generate fname = do r <- parseMsg fname
                    case r of
                      Left err -> do putStrLn $ "ERROR: " ++ err
                                     exitWith (ExitFailure (-2))
                      Right msg -> B.writeFile fname' (generateMsgType msg)
    where fname' = replaceExtension fname ".hs"

main = do args <- getArgs
          case args of
            [name] -> generate name
            _ -> do putStrLn "Expected 1 argument: path to .msg file"
                    exitWith (ExitFailure (-1))