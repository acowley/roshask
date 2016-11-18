{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Int32MultiArray where
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

data Int32MultiArray = Int32MultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                       , __data :: V.Vector P.Int
                                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Int32MultiArray)

instance RosBinary Int32MultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = Int32MultiArray <$> get <*> get

instance MsgInfo Int32MultiArray where
  sourceMD5 _ = "1d99f79f8b325b44fee908053e9c945b"
  msgTypeName _ = "std_msgs/Int32MultiArray"

instance D.Default Int32MultiArray

