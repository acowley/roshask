{-# LANGUAGE OverloadedStrings #-}
-- |ROS message types.
module Types (MsgType(..), MsgField(..), MsgConst(..),
              MsgName, msgName, requestMsgName, responseMsgName,
              shortName, rawName, fullRosMsgName,
              fullRosSrvName,
              Msg(..), hasHeader, Srv(..),
              requestResponseNames) where
import Data.ByteString.Char8 (ByteString)
import Data.Char (toUpper)

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

-- | A Haskell type name for a message definition.
data MsgName = MsgName { msgRawName :: String
                       , msgTypeName :: String } deriving Show

-- | Build a Haskell type name for a message. This ensures the name is
-- a valid Haskell type name.
msgName :: String -> MsgName
msgName [] = error "An empty message name is impossible!"
msgName n@(x:xs) = MsgName n (toUpper x : xs)

requestMsgName :: String -> MsgName
requestMsgName name = msgName $ name ++ "Request"

responseMsgName :: String -> MsgName
responseMsgName name = msgName $ name ++ "Response"

-- | Pull the Haskell type name for a message from a 'Msg'.
shortName :: Msg -> String
shortName = msgTypeName . shortTypeName

-- | Extract the original, raw, name of a message definition. This may
-- differ from the 'shortName' in that 'shortName' will always be
-- capitalized.
rawName :: Msg -> String
rawName = msgRawName . shortTypeName

-- | Get the full ROS name of a message type. This will be of the
-- form, @packageName/msgName@.
fullRosMsgName :: Msg -> String
fullRosMsgName m = msgPackage m ++ '/' : rawName m

fullRosSrvName :: Srv -> String
fullRosSrvName s = srvPackage s ++ '/' : (msgRawName . srvName) s

-- |A message has a short name, a long name, an md5 sum, and a list of
-- named, typed fields.
data Msg = Msg { shortTypeName :: MsgName
               , msgPackage    :: String
               , msgSource     :: ByteString
               , fields        :: [MsgField]
               , constants     :: [MsgConst] }

instance Show Msg where
    show (Msg sn ln _ f c) = unwords 
                             ["Msg", show sn, show ln, show f, show c]

hasHeader :: Msg -> Bool
hasHeader msg = case fields msg of
                  --((_, RUserType "Header"):_) -> True
                  (MsgField _ (RUserType "Header") _ : _) -> True
                  _ -> False

-- |A service has a request message, a response message, and a name.
data Srv = Srv { srvName :: MsgName
               , srvPackage :: String
               , srvSource :: ByteString
               , srvRequest :: Msg
               , srvResponse :: Msg
               }

instance Show Srv where
  show (Srv name package _ req res) =
    unwords ["Srv", show name, show package, show req, show res]

requestResponseNames :: Srv -> [String]
requestResponseNames srv = [reqName, resName]
  where
    reqName = shortName . srvRequest $ srv
    resName = shortName . srvResponse $ srv
