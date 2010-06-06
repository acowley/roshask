{-# LANGUAGE GADTs, ExistentialQuantification, ScopedTypeVariables #-}
-- ROS Node implementation.
module ROSTypes where
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
--import qualified Network.XmlRpc.Internals as XmlRpc
import Data.Typeable
import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan
--import System.IO (Handle)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary

import System.IO.Unsafe (unsafeInterleaveIO)
import Unsafe.Coerce

type URI       = String
type CallerID  = String
type TopicName = String

-- |A reflective interface for topics that names the type of data
-- transmitted over the topic and includes the MD5 hash of the .msg
-- file the type is derived from.
class TopicType t where
    name :: t -> String
    md5  :: t -> String

data Stream a = Stream a (Stream a)

--type Handler a b = Stream a -> Stream b

data Handler a b where
    Handler :: Stream a -> Stream b -> Handler a b

data EHandler = forall a b. EHandler (Handler a b)

--data EStream = forall a. Binary a => EStream (Stream a)

{-
data Node = Node { master        :: URI
                 , subscriptions :: Map TopicName (URI -> IO ())
                 , publications  :: Map TopicName (Handle -> IO ())
                 , threads       :: [ThreadId]
                 , handles       :: [Handle] }
-}

newtype RosInt = RosInt { unRosInt :: Int } deriving Show

instance Typeable RosInt where
    typeOf _ = mkTyConApp (mkTyCon "RosInt") []

instance Binary RosInt where
    put = putWord32host . unsafeCoerce . unRosInt
    get = unsafeCoerce <$> getWord32host

handleInt :: Int -> IO ()
handleInt x = putStrLn $ "Got int: " ++ show x

src1 :: Stream RosInt
src1 = listToStream $ cycle $ map RosInt [1,2,3]

listToStream (x:xs) = Stream x (listToStream xs)

data EStream = forall a. EChan (a -> IO ())

data Node = Node { master :: URI
                 , subscriptions :: Map TopicName EStream
                 , publications :: Map TopicName EStream }

testNode :: Node
testNode = Node "home" (M.fromList [("nums", (EChan handleInt))]) M.empty

--opaqueData :: B.ByteString
opaqueData = runPut $ do mapM_ put (map RosInt [1,2,3])






--, params :: Map String XmlRpc.Value }

