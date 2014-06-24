{-# LANGUAGE TemplateHaskell #-}
-- |Support for publishing log messages at various severity
-- levels. The log messages are annotated with the filename and line
-- number where they are generated.
module Ros.Logging (Log, LogLevel(..), enableLogging,
                    logDebug, logWarn, logInfo, logError, logFatal) where
import Control.Concurrent.Chan
import Control.Monad (when)
import Data.IORef
import Data.Word (Word8)
import Language.Haskell.TH
import System.IO.Unsafe

import Ros.Internal.Log (Log(Log))
import qualified Ros.Internal.Log as Log
import Ros.Internal.Header
import Ros.Node
import Ros.Topic.Util (fromList)

emptyHeader :: Header
emptyHeader = Header 0 (0,0) ""

mkLogMsg :: Word8 -> String -> Q Exp
mkLogMsg level msg = do Loc fname _ _ start _ <- location
                        let (line, _char) = start
                            litS = return . LitE . StringL
                            litI :: Integral a => a -> Q Exp
                            litI = return . LitE . IntegerL . fromIntegral
                        [|sendMsg (Log emptyHeader $(litI level) "" $(litS msg) 
                                       $(litS fname) "" $(litI line) [])|]

-- |Template Haskell functions to splice in a 'Log' value. Usage: 
-- 
-- > $(logDebug "This is my message to you")
logDebug, logWarn, logInfo, logError, logFatal :: String -> Q Exp
logDebug = mkLogMsg Log.dEBUG
logInfo  = mkLogMsg Log.iNFO
logWarn  = mkLogMsg Log.wARN
logError = mkLogMsg Log.eRROR
logFatal = mkLogMsg Log.fATAL

-- The 'Chan' into which all log messages are funneled. This Chan's
-- contents are fed into the /rosout 'Topic'.
rosOutChan :: Chan Log
rosOutChan = unsafePerformIO $ newChan
{-# NOINLINE rosOutChan #-}

-- Stash a function that knows whether or not 'Log' messages of
-- various levels should be printed to stdout.
showLevel :: IORef (Log -> IO ())
showLevel = unsafePerformIO $ newIORef (const (return ()))
{-# NOINLINE showLevel #-}

-- Stash the node name away so that it can be inserted into runtime
-- log messages.
nodeName :: IORef String
nodeName = unsafePerformIO $ newIORef ""
{-# NOINLINE nodeName #-}

-- Publish a log message.
sendMsg :: Log -> IO ()
sendMsg msg = do n <- readIORef nodeName
                 let msg' = msg { Log.name = n }
                 ($ msg') =<< readIORef showLevel
                 writeChan rosOutChan msg'

-- Prints messages whose level is greater than or equal to the
-- specified level.
printLog :: LogLevel -> Log -> IO ()
printLog lvl = let code = 2 ^ fromEnum lvl
               in \msg -> when (Log.level msg >= code) (putStrLn (show msg))

-- |Log message levels. These allow for simple filtering of messages.
data LogLevel = Debug | Info | Warn | Error | Fatal deriving (Eq, Enum)

-- |Enable logging for this node. The argument indicates the level of
-- log messages that should be echoed to standard out. If 'Nothing',
-- then no messages are printed; if 'Just lvl', then all messages of
-- greater than or equal level are printed.
enableLogging :: Maybe LogLevel -> Node ()
enableLogging ll = do xs <- liftIO $ getChanContents rosOutChan
                      liftIO $ maybe (return ())
                                     (writeIORef showLevel . printLog)
                                     ll
                      liftIO . writeIORef nodeName =<< getName
                      advertise "/rosout" (fromList xs)
