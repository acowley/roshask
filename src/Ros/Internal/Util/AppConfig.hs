-- |Support for read-only executable application configurations.
module Ros.Internal.Util.AppConfig where
import Control.Monad.Reader
import Control.Concurrent

data ConfigOptions = ConfigOptions { verbosity :: Int }

type Config = ReaderT ConfigOptions IO

getVerbosity :: Config Int
getVerbosity = verbosity `fmap` ask

debug :: String -> Config ()
debug s = do v <- getVerbosity
             when (v > 0) (liftIO (putStrLn s))

forkConfig :: Config () -> Config ThreadId
forkConfig c = do r <- ask
                  liftIO . forkIO $ runReaderT c r

parseAppConfig :: [String] -> (ConfigOptions, [String])
parseAppConfig args
  | "-v" `elem` args = (ConfigOptions 1, filter (`notElem` appOpts) args)
  | otherwise = (ConfigOptions 0, args)
  where appOpts = ["-v"]

configured :: Config a -> Config (IO a)
configured c = ask >>= return . runReaderT c