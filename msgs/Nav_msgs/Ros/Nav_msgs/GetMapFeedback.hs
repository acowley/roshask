{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GetMapFeedback where
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

data GetMapFeedback = GetMapFeedback deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary GetMapFeedback where
  put _  = pure ()
  get = pure GetMapFeedback

instance Storable GetMapFeedback where
  sizeOf _ = 1
  alignment _ = 1
  peek _ = pure GetMapFeedback
  poke _ _ = pure ()

instance MsgInfo GetMapFeedback where
  sourceMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  msgTypeName _ = "nav_msgs/GetMapFeedback"

instance D.Default GetMapFeedback

