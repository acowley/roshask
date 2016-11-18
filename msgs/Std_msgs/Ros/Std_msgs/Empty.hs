{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Empty where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Foreign.Storable (Storable(..))
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Empty = Empty deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary Empty where
  put _  = pure ()
  get = pure Empty

instance Storable Empty where
  sizeOf _ = 1
  alignment _ = 1
  peek _ = pure Empty
  poke _ _ = pure ()

instance MsgInfo Empty where
  sourceMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  msgTypeName _ = "std_msgs/Empty"

instance D.Default Empty

