{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Polygon where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Vector.Storable as V
import qualified Ros.Geometry_msgs.Point32 as Point32
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Polygon = Polygon { _points :: V.Vector Point32.Point32
                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Polygon)

instance RosBinary Polygon where
  put obj' = put (_points obj')
  get = Polygon <$> get

instance MsgInfo Polygon where
  sourceMD5 _ = "cd60a26494a087f577976f0329fa120e"
  msgTypeName _ = "geometry_msgs/Polygon"

instance D.Default Polygon

