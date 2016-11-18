{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.ByteMultiArray where
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

data ByteMultiArray = ByteMultiArray { _layout :: MultiArrayLayout.MultiArrayLayout
                                     , __data :: V.Vector Word.Word8
                                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''ByteMultiArray)

instance RosBinary ByteMultiArray where
  put obj' = put (_layout obj') *> put (__data obj')
  get = ByteMultiArray <$> get <*> get

instance MsgInfo ByteMultiArray where
  sourceMD5 _ = "70ea476cbcfd65ac2f68f3cda1e891fe"
  msgTypeName _ = "std_msgs/ByteMultiArray"

instance D.Default ByteMultiArray

