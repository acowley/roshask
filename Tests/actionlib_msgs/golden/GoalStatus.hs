{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
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

data GoalStatus = GoalStatus { goal_id :: GoalID.GoalID
                             , status :: Word.Word8
                             , text :: P.String
                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary GoalStatus where
  put obj' = put (goal_id obj') *> put (status obj') *> put (text obj')
  get = GoalStatus <$> get <*> get <*> get

instance MsgInfo GoalStatus where
  sourceMD5 _ = "d388f9b87b3c471f784434d671988d4a"
  msgTypeName _ = "actionlib_msgs/GoalStatus"

instance D.Default GoalStatus

pENDING :: Word.Word8
pENDING = 0

aCTIVE :: Word.Word8
aCTIVE = 1

pREEMPTED :: Word.Word8
pREEMPTED = 2

sUCCEEDED :: Word.Word8
sUCCEEDED = 3

aBORTED :: Word.Word8
aBORTED = 4

rEJECTED :: Word.Word8
rEJECTED = 5

pREEMPTING :: Word.Word8
pREEMPTING = 6

rECALLING :: Word.Word8
rECALLING = 7

rECALLED :: Word.Word8
rECALLED = 8

lOST :: Word.Word8
lOST = 9

