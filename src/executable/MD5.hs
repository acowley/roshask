{-# LANGUAGE OverloadedStrings #-}
module MD5 (msgMD5) where
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy (fromChunks)
import Data.Map ((!))
import qualified Data.Map as M
import Parse (simpleFieldAssoc)
import Types
import Analysis

tMap :: M.Map MsgType ByteString
tMap = M.fromList simpleFieldAssoc

typeText :: MsgType -> MsgInfo ByteString
typeText (RFixedArray n t) = 
  case M.lookup t tMap of
    Just t' -> return $ B.concat [t', "[", B.pack . show $ n, "]"]
    Nothing -> typeText t
typeText (RVarArray t) =
  case M.lookup t tMap of
    Just t' -> return $ B.append t' "[]"
    Nothing -> typeText t
typeText (RUserType b) = getMsg b >>= msgMD5 . snd >>= return . B.pack
typeText t = return $ tMap ! t

md5sum :: ByteString -> String
md5sum = show . md5 . fromChunks . (:[])

-- The "MD5 text" of a message is the .msg text with
-- * comments removed
-- * whitespace removed
-- * package names of dependencies removed
-- * constants reordered ahead of other declarations
-- from http://www.ros.org/wiki/ROS/Technical%20Overview
--
-- For user defined types, rather than recording a field as "type
-- identifier", we record "md5 identifier".
msgMD5 :: Msg -> MsgInfo String
msgMD5 m = let cs = map constantText (constants m)
           in withMsg m $
                do fs <- mapM fieldText (fields m)
                   return . md5sum $ B.intercalate "\n" (cs ++ fs)
  where constantText (MsgConst _ t v n) = B.concat [tMap ! t, " ", n, "=", v]
        fieldText (MsgField _ t n) = do t' <- typeText t
                                        return $ B.concat [t', " ", n]
