{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Test_msgs.StringConst where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data StringConst = StringConst { _link_name :: P.String
                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''StringConst)

instance RosBinary StringConst where
  put obj' = put (_link_name obj')
  get = StringConst <$> get

instance MsgInfo StringConst where
  sourceMD5 _ = "a8e1a25e612660c2e8d3d161e9e91950"
  msgTypeName _ = "test_msgs/StringConst"

instance D.Default StringConst

remove_all_attached_objects :: P.String
remove_all_attached_objects = "\"all\""

embedded_quotes :: P.String
embedded_quotes = "here \"we\" go"

