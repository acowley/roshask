{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Ros.Internal.Log where
import qualified Prelude as P
import Prelude ((.))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import Ros.Internal.Msg.HeaderSupport
import qualified Data.Word as Word
import qualified Ros.Internal.Header as Header

data Log = Log { header   :: Header.Header
               , level    :: Word.Word8
               , name     :: P.String
               , msg      :: P.String
               , file     :: P.String
               , function :: P.String
               , line     :: Word.Word32
               , topics   :: [P.String]
               } deriving (P.Show, P.Eq, P.Ord, T.Typeable)

instance RosBinary Log where
  put obj' = put (header obj') *> put (level obj') *> put (name obj') *> put (msg obj') *> put (file obj') *> put (function obj') *> put (line obj') *> putList (topics obj')
  get = Log <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> getList
  putMsg = putStampedMsg

instance HasHeader Log where
  getSequence = Header.seq . header
  getFrame = Header.frame_id . header
  getStamp = Header.stamp . header
  setSequence seq x' = x' { header = (header x') { Header.seq = seq } }

instance MsgInfo Log where
  sourceMD5 _ = "acffd30cd6b6de30f120938c17c593fb"
  msgTypeName _ = "rosgraph_msgs/Log"

dEBUG :: Word.Word8
dEBUG = 1

iNFO :: Word.Word8
iNFO = 2

wARN :: Word.Word8
wARN = 4

eRROR :: Word.Word8
eRROR = 8

fATAL :: Word.Word8
fATAL = 16
