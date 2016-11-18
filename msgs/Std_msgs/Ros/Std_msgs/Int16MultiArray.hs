{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Int16MultiArray where
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

data Int16MultiArray = Int16MultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                       , __data :: V.Vector Int.Int16
                                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Int16MultiArray)

instance RosBinary Int16MultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = Int16MultiArray <$> get <*> get

instance MsgInfo Int16MultiArray where
  sourceMD5 _ = "d9338d7f523fcb692fae9d0a0e9f067c"
  msgTypeName _ = "std_msgs/Int16MultiArray"

instance D.Default Int16MultiArray

