module Ros.Logging (Log(..), 
                    logDebug, logWarn, logInfo, logError, logFatal) where
import Data.Word (Word8)
import Ros.Roslib.Log
import Ros.Roslib.Header

emptyHeader :: Header
emptyHeader = Header 0 (0,0) ""

mkLogMsg :: Word8 -> String -> Log
mkLogMsg level msg = Log emptyHeader level msg "" "" 0 []

logDebug, logWarn, logInfo, logError, logFatal :: String -> Log
logDebug = mkLogMsg dEBUG
logWarn  = mkLogMsg wARN
logInfo  = mkLogMsg iNFO
logError = mkLogMsg eRROR
logFatal = mkLogMsg fATAL
