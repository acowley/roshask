{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Header where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.RosTypes
import qualified Data.Word as Word
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Header = Header { _seq :: Word.Word32
                     , _stamp :: ROSTime
                     , _frame_id :: P.String
                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Header)

instance RosBinary Header where
  put obj' = put (_seq obj') *> put (_stamp obj') *> put (_frame_id obj')
  get = Header <$> get <*> get <*> get

instance MsgInfo Header where
  sourceMD5 _ = "2176decaecbce78abc3b96ef049fabed"
  msgTypeName _ = "std_msgs/Header"

instance D.Default Header

