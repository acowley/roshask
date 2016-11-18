{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Float32MultiArray where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Vector.Storable as V
import qualified Ros.Std_msgs.MultiArrayLayout as MultiArrayLayout
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Float32MultiArray = Float32MultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                           , __data :: V.Vector P.Float
                                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Float32MultiArray)

instance RosBinary Float32MultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = Float32MultiArray <$> get <*> get

instance MsgInfo Float32MultiArray where
  sourceMD5 _ = "6a40e0ffa6a17a503ac3f8616991b1f6"
  msgTypeName _ = "std_msgs/Float32MultiArray"

instance D.Default Float32MultiArray

