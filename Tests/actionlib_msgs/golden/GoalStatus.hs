{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Actionlib_msgs.GoalStatus where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Word as Word
import qualified Ros.Actionlib_msgs.GoalID as GoalID
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GoalStatus = GoalStatus { _goal_id :: GoalID.GoalID
                             , _status :: Word.Word8
                             , _text :: P.String
                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GoalStatus)

instance RosBinary GoalStatus where
  put obj' = put (_goal_id obj') *> put (_status obj') *> put (_text obj')
  get = GoalStatus <$> get <*> get <*> get

instance MsgInfo GoalStatus where
  sourceMD5 _ = "d388f9b87b3c471f784434d671988d4a"
  msgTypeName _ = "actionlib_msgs/GoalStatus"

instance D.Default GoalStatus

pending :: Word.Word8
pending = 0

active :: Word.Word8
active = 1

preempted :: Word.Word8
preempted = 2

succeeded :: Word.Word8
succeeded = 3

aborted :: Word.Word8
aborted = 4

rejected :: Word.Word8
rejected = 5

preempting :: Word.Word8
preempting = 6

recalling :: Word.Word8
recalling = 7

recalled :: Word.Word8
recalled = 8

lost :: Word.Word8
lost = 9

