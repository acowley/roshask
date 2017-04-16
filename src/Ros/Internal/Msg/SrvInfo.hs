module Ros.Internal.Msg.SrvInfo where

class SrvInfo a where
    srvMD5 :: a -> String
    srvTypeName :: a -> String
