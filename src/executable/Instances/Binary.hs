{-# LANGUAGE OverloadedStrings #-}
-- |Generate a Binary instance for ROS msg types.
module Instances.Binary (genBinaryInstance) where
import Control.Monad ((>=>))
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Types
import Analysis

serialize :: MsgType -> MsgInfo ByteString
serialize = getTypeInfo >=> return . putField

deserialize :: MsgType -> MsgInfo ByteString
deserialize = getTypeInfo >=> return . getField

putMsgHeader :: ByteString
putMsgHeader = "\n  putMsg = putStampedMsg"

genBinaryInstance :: Msg -> MsgInfo ByteString
genBinaryInstance m 
  | null (fields m) = return $ 
                      B.concat [ "instance RosBinary ", name'
                               , " where\n"
                               , "  put _  = putUnit\n"
                               , "  get = pure ", name']
  | otherwise = 
    do puts <- mapM (\f -> serialize (fieldType f) >>= 
                           return . buildPut (fieldName f)) 
                    (fields m)
       gets <- mapM (deserialize . fieldType) (fields m)
       return $ B.concat ["instance RosBinary ", name', " where\n",
                          "  put obj' = ", B.intercalate " *> " puts,"\n",
                          "  get = ", name', " <$> ",
                          B.intercalate " <*> " gets,
                          if hasHeader m then putMsgHeader else ""]
    where buildPut f ser = B.concat [ser, " (", f, " obj')"]
          name' = pack (shortName m)
