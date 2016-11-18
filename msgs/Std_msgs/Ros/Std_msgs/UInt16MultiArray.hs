{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.UInt16MultiArray where
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

data UInt16MultiArray = UInt16MultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                         , __data :: V.Vector Word.Word16
                                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''UInt16MultiArray)

instance RosBinary UInt16MultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = UInt16MultiArray <$> get <*> get

instance MsgInfo UInt16MultiArray where
  sourceMD5 _ = "52f264f1c973c4b73790d384c6cb4484"
  msgTypeName _ = "std_msgs/UInt16MultiArray"

instance D.Default UInt16MultiArray

