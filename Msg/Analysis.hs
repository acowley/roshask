module Msg.Analysis (isMsgFlat) where
import Msg.Types
import Msg.Parse
import Ros.Build.DepFinder (findMessage)
import Data.ByteString.Char8 (unpack)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p [] = return True
allM p (x:xs) = p x >>= aux
    where aux True = allM p xs
          aux False = return False
          
isMsgFlat :: Msg -> IO Bool
isMsgFlat (Msg _ msgName _ fields) = allM flat $ map snd fields
    where flat (RVarArray _) = return False
          flat (RUserType n) = either error isMsgFlat =<< 
                               maybe (err n) parseMsg =<< 
                               findMessage homePkg (unpack n)
          flat _             = return True
          homePkg = fst $ span (/= '/') msgName
          err n = error $ "Couldn't find user-defined message type " ++ unpack n