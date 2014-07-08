{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
module Ros.Test_msgs.StringConst where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D

data StringConst = StringConst { link_name :: P.String
                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary StringConst where
  put obj' = put (link_name obj')
  get = StringConst <$> get

instance MsgInfo StringConst where
  sourceMD5 _ = "a8e1a25e612660c2e8d3d161e9e91950"
  msgTypeName _ = "test_msgs/StringConst"

instance D.Default StringConst

rEMOVE_ALL_ATTACHED_OBJECTS :: P.String
rEMOVE_ALL_ATTACHED_OBJECTS = "\"all\""

eMBEDDED_QUOTES :: P.String
eMBEDDED_QUOTES = "here \"we\" go"
