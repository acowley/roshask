{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Msg.Analysis (MsgInfo, getTypeInfo, analyzeMsg, isFlat, 
                     SerialInfo(..)) where
import Control.Applicative
import Control.Arrow (first, second)
import "monads-fd" Control.Monad.State
import Data.ByteString.Char8 (pack, unpack, ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import Data.Map (Map, insert, notMember, union, (!))
import qualified Data.Map as M
import Data.Maybe (isJust)
import System.FilePath (takeFileName)

import Msg.Types
import Msg.Parse
import Ros.Build.DepFinder (findMessage)

-- The context in which message types are resolved is a pair of a
-- current home package (used to lookup unqualified message type
-- names) and a mapping from previously encountered message types to
-- serial info.
type MsgContext = (String, Map MsgType SerialInfo)

-- Code snippets for use in assembling a Haskell declaration (using
-- the hType field that represents a Haskell type), RosBinary and,
-- optionally, Storable instances for a message type that contains a
-- field of this type.
data SerialInfo = SerialInfo { hType    :: ByteString
                             , putField :: ByteString
                             , getField :: ByteString
                             , size     :: Maybe ByteString }

type MsgInfo = StateT MsgContext IO

analyzeMsg :: Msg -> MsgInfo a -> IO a
analyzeMsg msg action = evalStateT (addMsg msg >> action) ("", M.empty)

getHomePkg :: MsgInfo String
getHomePkg = fst <$> get

setHomePkg :: String -> MsgInfo ()
setHomePkg = modify . first . const

setTypeInfo :: MsgType -> SerialInfo -> MsgInfo SerialInfo
setTypeInfo mt info = modify (second $ insert mt info) >> return info

getTypeInfo :: MsgType -> MsgInfo SerialInfo
getTypeInfo mt = aux . M.lookup mt . snd =<< get
    where aux Nothing = addField mt
          aux (Just info) = return info

isFlat :: MsgInfo Bool
isFlat = F.all isStorable . snd <$> get

isStorable :: SerialInfo -> Bool
isStorable = isJust . size

-- Add bindings for every MsgType referenced by this Msg to a Haskell
-- type and serialization information for that type to a 'MsgInfo'
-- context.
addMsg :: Msg -> MsgInfo ()
addMsg (Msg _ name _ fields _) = do setHomePkg (takeWhile (/= '/') name)
                                    mapM_ getTypeInfo (map snd fields)


-- Default SerialInfo value for inductive (non-flat) values
defaultNonFlat :: ByteString -> SerialInfo
defaultNonFlat t = SerialInfo t "put" "get" Nothing

-- Default SerialInfo for a flat value.
defaultFlat :: ByteString -> SerialInfo
defaultFlat t = SerialInfo t "put" "get" (Just si)
    where si = B.concat ["sizeOf (P.undefined::", t, ")"]

-- Build a RHS for a sizeOf definition that multiplies a Storable
-- element's size by the number of elements a FixedArray of that type
-- contains.
mulSize :: Int -> SerialInfo -> ByteString
mulSize n (SerialInfo _ _ _ (Just sz)) = 
    B.concat [pack $ show n, " * (", sz, ")"]
mulSize n (SerialInfo _ _ _ Nothing) = 
    error "Can't generate a Storable instance for a RFixedArray\
          \with a non-Storable element type."

-- Add a SerialInfo value for a 'MsgType' to the 'MsgInfo' context.
addField :: MsgType -> MsgInfo SerialInfo
addField RString = ros2Hask RString >>= setTypeInfo RString . defaultNonFlat
addField t@(RVarArray el) = 
    do elInfo <- getTypeInfo el
       arr <- if isStorable elInfo
              then do lst <- vecOf el
                      return $ SerialInfo lst "put" "get" Nothing
              else do lst <- listOf el
                      return $ SerialInfo lst "putList" "getList" Nothing
       setTypeInfo t arr
addField t@(RFixedArray n el) = 
    do elInfo <- getTypeInfo el
       arr <- if isStorable elInfo
              then do lst <- vecOf el
                      return $ SerialInfo lst "put" "get" 
                                          (Just $ mulSize n elInfo)
              else do lst <- listOf el
                      return $ SerialInfo lst "putFixedList" "getFixedList"
                                          Nothing
       setTypeInfo t arr
addField t@(RUserType n) = do msg <- getMsg n
                              s' <- liftIO . runEnriched msg . snd =<< get
                              modify (second $ union (snd s'))
                              userFlat <- liftIO $ evalStateT isFlat s'
                              t' <- ros2Hask t
                              setTypeInfo t $ (if userFlat
                                               then defaultFlat
                                               else defaultNonFlat) t'
    where runEnriched msg = execStateT (addMsg msg) . 
                            (\x -> ("",x)) . M.filter isStorable
addField t = ros2Hask t >>= setTypeInfo t . defaultFlat

-- Generate the name of the Haskell type that corresponds to a flat
-- (i.e. non-array) ROS type.
mkFlatType :: MsgType -> ByteString
mkFlatType RBool         = "P.Bool"
mkFlatType RInt8         = "Int.Int8"
mkFlatType RUInt8        = "Word.Word8"
mkFlatType RInt16        = "Int.Int16"
mkFlatType RUInt16       = "Word.Word16"
mkFlatType RInt32        = "P.Int"
mkFlatType RUInt32       = "Word.Word32"
mkFlatType RInt64        = "Int.Int64"
mkFlatType RUInt64       = "Word.Word64"
mkFlatType RFloat32      = "P.Float"
mkFlatType RFloat64      = "P.Double"
mkFlatType RTime         = "ROSTime"
mkFlatType RDuration     = "ROSDuration"
mkFlatType (RUserType t) = qualify . pack . takeFileName . unpack $ t
    where qualify b = B.concat [b, ".", b]
mkFlatType t             = error $ show t ++ " is not a flat type"

-- Given a home package name and a ROS 'MsgType', generate a Haskell
-- type name.
ros2Hask :: MsgType -> MsgInfo ByteString
ros2Hask (RFixedArray _ t) = mkArrayType t
ros2Hask (RVarArray t)     = mkArrayType t
ros2Hask RString           = return "P.String"
ros2Hask t                 = return $ mkFlatType t

vecOf :: MsgType -> MsgInfo ByteString
vecOf = getTypeInfo >=> return . B.append "V.Vector " . hType

listOf :: MsgType -> MsgInfo ByteString
listOf = getTypeInfo >=> return . buildString . hType
    where buildString t = B.concat ["[", t, "]"]

-- Make an array type declaration. If the element type of the
-- collection is flat (i.e. does not itself have a field of type
-- 'RVarArray'), then it will have a 'Storable' instance and can be
-- stored in a 'Vector'.
mkArrayType :: MsgType -> MsgInfo ByteString
mkArrayType t = toArr . isStorable =<< getTypeInfo t
    where toArr True = vecOf t
          toArr False = listOf t

-- Find a message by name
getMsg :: ByteString -> MsgInfo Msg
getMsg n = do homePkg <- getHomePkg
              liftIO $ (do file <- findMessage homePkg (unpack n)
                           msg <- maybe (err n) parseMsg file
                           either error return msg)
    where err n = error $ "Couldn't find user-defined message type " ++ unpack n
