{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Ros.Internal.Header where
import qualified Prelude as P
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import Ros.Internal.RosTypes
import qualified Data.Word as Word

data Header = Header { seq :: Word.Word32
                     , stamp :: ROSTime
                     , frame_id :: P.String
                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable)

instance RosBinary Header where
  put obj' = put (seq obj') *> put (stamp obj') *> put (frame_id obj')
  get = Header <$> get <*> get <*> get

instance MsgInfo Header where
  sourceMD5 _ = "2176decaecbce78abc3b96ef049fabed"
  msgTypeName _ = "std_msgs/Header"
