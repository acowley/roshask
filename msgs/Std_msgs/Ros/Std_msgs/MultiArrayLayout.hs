{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.MultiArrayLayout where
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
import qualified Ros.Std_msgs.MultiArrayDimension as MultiArrayDimension
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data MultiArrayLayout = MultiArrayLayout { _dim :: [MultiArrayDimension.MultiArrayDimension]
                                         , _data_offset :: Word.Word32
                                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''MultiArrayLayout)

instance RosBinary MultiArrayLayout where
  put obj' = putList (_dim obj') *> put (_data_offset obj')
  get = MultiArrayLayout <$> getList <*> get

instance MsgInfo MultiArrayLayout where
  sourceMD5 _ = "0fed2a11c13e11c5571b4e2a995a91a3"
  msgTypeName _ = "std_msgs/MultiArrayLayout"

instance D.Default MultiArrayLayout

