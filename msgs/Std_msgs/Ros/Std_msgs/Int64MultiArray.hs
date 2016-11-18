{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Int64MultiArray where
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

data Int64MultiArray = Int64MultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                       , __data :: V.Vector Int.Int64
                                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Int64MultiArray)

instance RosBinary Int64MultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = Int64MultiArray <$> get <*> get

instance MsgInfo Int64MultiArray where
  sourceMD5 _ = "54865aa6c65be0448113a2afc6a49270"
  msgTypeName _ = "std_msgs/Int64MultiArray"

instance D.Default Int64MultiArray

