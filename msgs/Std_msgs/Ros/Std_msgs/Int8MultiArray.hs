{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Int8MultiArray where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Int as Int
import qualified Data.Vector.Storable as V
import qualified Ros.Std_msgs.MultiArrayLayout as MultiArrayLayout
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Int8MultiArray = Int8MultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                     , __data :: V.Vector Int.Int8
                                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Int8MultiArray)

instance RosBinary Int8MultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = Int8MultiArray <$> get <*> get

instance MsgInfo Int8MultiArray where
  sourceMD5 _ = "d7c1af35a1b4781bbe79e03dd94b7c13"
  msgTypeName _ = "std_msgs/Int8MultiArray"

instance D.Default Int8MultiArray

