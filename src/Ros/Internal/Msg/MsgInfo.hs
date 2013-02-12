module Ros.Internal.Msg.MsgInfo where

class MsgInfo a where
    sourceMD5 :: a -> String
    msgTypeName :: a -> String