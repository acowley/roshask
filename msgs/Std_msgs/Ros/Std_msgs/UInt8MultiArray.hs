{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.UInt8MultiArray where
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

data UInt8MultiArray = UInt8MultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                       , __data :: V.Vector Word.Word8
                                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''UInt8MultiArray)

instance RosBinary UInt8MultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = UInt8MultiArray <$> get <*> get

instance MsgInfo UInt8MultiArray where
  sourceMD5 _ = "82373f1612381bb6ee473b5cd6f5d89c"
  msgTypeName _ = "std_msgs/UInt8MultiArray"

instance D.Default UInt8MultiArray

