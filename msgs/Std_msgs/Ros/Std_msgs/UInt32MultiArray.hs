{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.UInt32MultiArray where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Vector.Storable as V
import qualified Data.Word as Word
import qualified Ros.Std_msgs.MultiArrayLayout as MultiArrayLayout
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data UInt32MultiArray = UInt32MultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                         , __data :: V.Vector Word.Word32
                                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''UInt32MultiArray)

instance RosBinary UInt32MultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = UInt32MultiArray <$> get <*> get

instance MsgInfo UInt32MultiArray where
  sourceMD5 _ = "4d6a180abc9be191b96a7eda6c8a233d"
  msgTypeName _ = "std_msgs/UInt32MultiArray"

instance D.Default UInt32MultiArray

