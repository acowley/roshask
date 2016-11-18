{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.MultiArrayDimension where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Word as Word
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data MultiArrayDimension = MultiArrayDimension { _label :: P.String
                                               , _size :: Word.Word32
                                               , _stride :: Word.Word32
                                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''MultiArrayDimension)

instance RosBinary MultiArrayDimension where
  put obj' = put (_label obj') *> put (_size obj') *> put (_stride obj')
  get = MultiArrayDimension <$> get <*> get <*> get

instance MsgInfo MultiArrayDimension where
  sourceMD5 _ = "4cd0c83a8683deae40ecdac60e53bfa8"
  msgTypeName _ = "std_msgs/MultiArrayDimension"

instance D.Default MultiArrayDimension

