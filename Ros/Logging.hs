{-# LANGUAGE TemplateHaskell #-}
-- |Support for publishing log messages at various severity
-- levels. The log messages are annotated with the filename and line
-- number where they are generated.
module Ros.Logging (Log(..), 
                    logDebug, logWarn, logInfo, logError, logFatal) where
import Data.Word (Word8)
import Ros.Roslib.Log
import Ros.Roslib.Header
import Language.Haskell.TH

emptyHeader :: Header
emptyHeader = Header 0 (0,0) ""

mkLogMsg :: Word8 -> String -> Q Exp
mkLogMsg level msg = do Loc fname _ _ start _ <- location
                        let (line, _char) = start
                            litS = return . LitE . StringL
                            litI :: Integral a => a -> Q Exp
                            litI = return . LitE . IntegerL . fromIntegral
                        [|Log emptyHeader $(litI level) "" $(litS msg) 
                              $(litS fname) "" $(litI line) []|]

-- |Template Haskell functions to splice in a 'Log' value. Usage: 
-- 
-- > $(logDebug "This is my message to you")
logDebug, logWarn, logInfo, logError, logFatal :: String -> Q Exp
logDebug = mkLogMsg dEBUG
logWarn  = mkLogMsg wARN
logInfo  = mkLogMsg iNFO
logError = mkLogMsg eRROR
logFatal = mkLogMsg fATAL

