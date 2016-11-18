{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.UInt64MultiArray where
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

data UInt64MultiArray = UInt64MultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                         , __data :: V.Vector Word.Word64
                                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''UInt64MultiArray)

instance RosBinary UInt64MultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = UInt64MultiArray <$> get <*> get

instance MsgInfo UInt64MultiArray where
  sourceMD5 _ = "6088f127afb1d6c72927aa1247e945af"
  msgTypeName _ = "std_msgs/UInt64MultiArray"

instance D.Default UInt64MultiArray

