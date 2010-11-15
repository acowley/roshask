{-# LANGUAGE OverloadedStrings #-}
-- |Generate a Binary instance for ROS msg types.
module Ros.Core.Msg.BinaryInstance (genBinaryInstance) where
import Control.Monad ((>=>))
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Ros.Core.Msg.Types
import Ros.Core.Msg.Analysis

serialize :: MsgType -> MsgInfo ByteString
serialize = getTypeInfo >=> return . putField

deserialize :: MsgType -> MsgInfo ByteString
deserialize = getTypeInfo >=> return . getField

putMsgHeader :: ByteString
putMsgHeader = "\n  putMsg = putStampedMsg"

genBinaryInstance :: Msg -> MsgInfo ByteString
genBinaryInstance m@(Msg name _ _ fields _) = 
    do puts <- mapM (\(f,t) -> serialize t >>= return . buildPut f) fields
       gets <- mapM (deserialize . snd) fields
       return $ B.concat ["instance RosBinary ", pack name, " where\n",
                          "  put obj' = ", B.intercalate " *> " puts,"\n",
                          "  get = ", pack name, " <$> ", 
                          B.intercalate " <*> " gets,
                          if hasHeader m then putMsgHeader else ""]
    where buildPut f ser = B.concat [ser, " (", f, " obj')"]
