{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Float64MultiArray where
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

data Float64MultiArray = Float64MultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                           , __data :: V.Vector P.Double
                                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Float64MultiArray)

instance RosBinary Float64MultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = Float64MultiArray <$> get <*> get

instance MsgInfo Float64MultiArray where
  sourceMD5 _ = "4b7d974086d4060e7db4613a7e6c3ba4"
  msgTypeName _ = "std_msgs/Float64MultiArray"

instance D.Default Float64MultiArray

