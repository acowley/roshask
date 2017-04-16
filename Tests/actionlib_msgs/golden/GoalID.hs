{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
module Ros.Actionlib_msgs.GoalID where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.RosTypes

data GoalID = GoalID { stamp :: ROSTime
                     , id :: P.String
                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary GoalID where
  put obj' = put (stamp obj') *> put (id obj')
  get = GoalID <$> get <*> get

instance MsgInfo GoalID where
  sourceMD5 _ = "302881f31927c1df708a2dbab0e80ee8"
  msgTypeName _ = "actionlib_msgs/GoalID"

instance D.Default GoalID

