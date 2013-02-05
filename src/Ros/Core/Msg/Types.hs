{-# LANGUAGE OverloadedStrings #-}
-- |ROS message types.
module Ros.Core.Msg.Types where
import Data.ByteString.Char8 (ByteString)
import Data.List (intercalate)

-- |A variant type describing the types that may be included in a ROS
-- message.
data MsgType = RBool | RInt8 | RUInt8 | RInt16 | RUInt16
             | RInt32 | RUInt32 | RInt64 | RUInt64
             | RFloat32 | RFloat64 | RString | RTime | RDuration
             | RFixedArray Int MsgType | RVarArray MsgType
             | RUserType ByteString | RByte | RChar
               deriving (Show, Eq, Ord)

data MsgField = MsgField { fieldName    :: ByteString
                         , fieldType    :: MsgType
                         , rawFieldName :: ByteString }
                deriving Show

data MsgConst = MsgConst { constName    :: ByteString
                         , constType    :: MsgType
                         , rawValue     :: ByteString 
                         , rawConstName :: ByteString }
                deriving Show

-- |A message has a short name, a long name, an md5 sum, and a list of
-- named, typed fields.
data Msg = Msg { shortName :: String
               , longName  :: String
               --, md5sum    :: IO String
               , txt       :: ByteString
               , fields    :: [MsgField]
               , constants :: [MsgConst] }

instance Show Msg where
    show (Msg sn ln _ f c) = intercalate " " 
                             ["Msg", show sn, show ln, show f, show c]

hasHeader :: Msg -> Bool
hasHeader msg = case fields msg of
                  --((_, RUserType "Header"):_) -> True
                  (MsgField _ (RUserType "Header") _ : _) -> True
                  _ -> False
