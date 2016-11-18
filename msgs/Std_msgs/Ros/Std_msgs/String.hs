{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.String where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data String = String { __data :: P.String
                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''String)

instance RosBinary String where
  put obj' = put (__data obj')
  get = String <$> get

instance MsgInfo String where
  sourceMD5 _ = "992ce8a1687cec8c8bd883ec73ca41d1"
  msgTypeName _ = "std_msgs/String"

instance D.Default String

