-- |The ROS connection header contains important metadata about a
-- connection being established, including typing information and
-- routing information. How it is exchanged depends on the ROS
-- transport being used.
module Ros.Node.ConnectionHeader (genHeader, ConnHeader(..), parseHeader) where
import Control.Applicative ((<$>))
import Control.Arrow (second, (***))
import Data.Binary.Get (getWord32le, Get, getByteString, runGetIncremental)
import qualified Data.Binary.Get as G
import Data.Binary.Put (runPut, putWord32le)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

-- |Wrapper for the Connection Header type that is a list of key-value
-- pairs.
newtype ConnHeader = ConnHeader [(String,String)]

-- |Serialize a list of key-value pairs using the ROS Connection
-- Header protocol.
genHeader :: [(String,String)] -> ByteString
genHeader = tagLength . 
            B.concat . 
            map (tagLength . uncurry B.append . 
                 second (B.cons '=') . (pack *** pack))

-- Prefix a ByteString with its length encoded as a 4-byte little
-- endian integer.
tagLength :: ByteString -> ByteString
tagLength x = let len = runPut $ putWord32le (fromIntegral (B.length x))
              in B.append (BL.toStrict len) x

getInt :: Get Int
getInt = fromIntegral <$> getWord32le

-- Each entry in the header is a 4-byte little endian integer denoting
-- the length of the entry, the field name string, an equals sign, and
-- the field value string.
parsePair :: Get (String,String)
parsePair = do len <- fromIntegral <$> getInt
               field <- getByteString len
               case B.elemIndex '=' field of
                 Just i -> return . (unpack *** unpack . B.tail) . B.splitAt i $
                           field
                 Nothing -> error "Didn't find '=' in connection header field"

-- Keep parsing header entries until we run out of bytes.
parseHeader :: ByteString -> [(String, String)]
parseHeader bs | B.null bs = []
               | otherwise = let G.Partial k = runGetIncremental parsePair
                                 G.Done rst _ p = k (Just bs)
                             in p : parseHeader rst
