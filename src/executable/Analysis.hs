{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Analysis (MsgInfo, liftIO, getTypeInfo, withMsg, getMsg, addMsg,
                 runAnalysis, isFlat, SerialInfo(..)) where
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.State
import Data.ByteString.Char8 (pack, unpack, ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe (isJust)
import System.FilePath (takeFileName, dropExtension)
import Ros.Internal.DepFinder (findMessagesInPkg)
import Types hiding (msgName)
import Parse
import ResolutionTypes

-- Synonym for a Msg paired with its SerialInfo. This tuple is cached
-- for message types as they are visited.
type SerialMsg = (SerialInfo, Msg)

-- Front-end to run analyses.
runAnalysis :: MsgInfo a -> IO a
runAnalysis m = evalStateT (cachePackage "rosgraph_msgs" >> m) emptyMsgContext

-- All the .msg files in a package are cached for quick lookup on
-- subsequent type resolutions.
cachePackage :: ByteString -> MsgInfo PkgCache
cachePackage pkgName = do (dir, msgs) <- liftIO $ findMessagesInPkg (unpack pkgName)
                          let cache = M.fromList (map prepMsg msgs)
                              pkg = (dir, M.empty, cache)
                          alterPkgMap (M.insert pkgName pkg)
                          return pkg
    where prepMsg = pack . dropExtension . takeFileName &&& Left

-- Get a package's 'MsgCache' from the package cache.
getPackage :: ByteString -> MsgInfo PkgCache
getPackage pkgName = 
    do pkgs <- msgDefs <$> get
       maybe (cachePackage pkgName) return $ M.lookup pkgName pkgs

-- Try to get a 'Msg' from a given package. The first argument is the
-- package name, the second argument is the unqualified message name.
getMsgFromPkg :: ByteString -> ByteString -> MsgInfo (Maybe SerialMsg)
getMsgFromPkg pkgName msgName = getPackage pkgName >>= lookupMsg . msgCache
    where lookupMsg :: MsgCache -> MsgInfo (Maybe SerialMsg)
          lookupMsg cache = maybe (return Nothing) 
                                  (return . Just <=< loadMsg)
                                  (M.lookup msgName cache)
          loadMsg :: (Either FilePath SerialMsg) -> MsgInfo SerialMsg
          loadMsg (Left fp) = either error addMsg =<< liftIO (parseMsg fp)
          loadMsg (Right m) = return m
          msgCache (_,_,c)  = c

getMsg :: ByteString -> MsgInfo SerialMsg
getMsg msgName = check <$>
                 if B.null msgType
                 then getMsgFromPkg "rosgraph_msgs" msgName <||>
                      getMsgFromPkg "std_msgs" msgName <||>
                      (flip getMsgFromPkg msgName . homePkg =<< get)
                 else getMsgFromPkg msgPkg (B.tail msgType)
    where (msgPkg, msgType) = B.span (/= '/') msgName
          check :: Maybe SerialMsg -> SerialMsg
          check Nothing = error $ "Couldn't resolve type " ++ unpack msgName
          check (Just m) = m
          -- checkLocal :: Maybe SerialMsg -> MsgInfo (Maybe SerialMsg)
          -- checkLocal Nothing = do home <- homePkg <$> get
          --                         getMsgFromPkg home msgName
          -- checkLocal info    = return info
          (<||>) :: (Applicative f, Alternative g) => f (g a) -> f (g a) -> f (g a)
          (<||>) = liftA2 (<|>)

isFlat :: Msg -> MsgInfo Bool
isFlat = fmap (all isStorable) . mapM (getTypeInfo . fieldType) . fields

isStorable :: SerialInfo -> Bool
isStorable = isJust . size

-- Add bindings for every MsgType referenced by this Msg to a Haskell
-- type and serialization information for that type to a 'MsgInfo'
-- context.
addMsg :: Msg -> MsgInfo SerialMsg
addMsg msg = do oldHome <- homePkg <$> get
                let pkgName = B.pack $ msgPackage msg
                    sName = pack $ shortName msg
                    tName = B.concat [sName, ".", sName]
                setHomePkg pkgName
                flat <- isFlat msg
                let ser = (if flat then defaultFlat else defaultNonFlat) $ tName
                addParsedMsg pkgName sName ser msg
                when (not (B.null oldHome)) (setHomePkg oldHome)
                return (ser,msg)

withMsg :: Msg -> MsgInfo a -> MsgInfo a
withMsg msg action = do _ <- addMsg msg
                        oldHome <- homePkg <$> get
                        setHomePkg . B.pack $ msgPackage msg
                        r <- action
                        setHomePkg oldHome
                        return r

-- Get a 'SerialInfo' value for a given 'MsgType'. If the specified
-- 'MsgType' has not yet been parsed, it will be resolved and parsed.
getTypeInfo :: MsgType -> MsgInfo SerialInfo
getTypeInfo mt = do pkg <- typeCache <$> (getPackage . homePkg =<< get)
                    aux . M.lookup mt $ pkg
    where aux Nothing = addField mt
          aux (Just info) = return info
          typeCache (_,tc,_) = tc

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
mulSize _ (SerialInfo _ _ _ Nothing) = 
    error "Can't generate a Storable instance for a RFixedArray\
          \with a non-Storable element type."

-- NOTE: ROS specifies that we serialize booleans as a single byte,
-- while there is an instance of Haskell's 'Storable' class for 'Bool'
-- that uses four bytes for each boolean value. We handle individual
-- boolean values with the 'RosBinary' instance for the Haskell 'Bool'
-- type (i.e. one byte for each boolean). We must handle arrays of
-- booleans specially as the serialized form must still be one byte
-- per value.

-- | Deserialization source code string to read a vector of bytes,
-- then convert that to a vector of 'Bool's.
getBoolFromWord :: ByteString
getBoolFromWord = "P.fmap (V.map (P.> 0) :: V.Vector Word.Word8 \
                  \-> V.Vector P.Bool) get"

-- | Serialization source code string to convert a vector of 'Bool's
-- to a vector of bytes before serializing.
putWordFromBool :: ByteString
putWordFromBool = "(put . (V.map (P.fromIntegral . P.fromEnum)\
                  \ :: V.Vector P.Bool -> V.Vector Word.Word8))"

-- Add a SerialInfo value for a 'MsgType' to the 'MsgInfo' context.
addField :: MsgType -> MsgInfo SerialInfo
addField RString = ros2Hask RString >>= setTypeInfo RString . defaultNonFlat
addField t@(RVarArray RBool) =
  do lst <- vecOf RBool
     let arr = SerialInfo lst putWordFromBool getBoolFromWord Nothing
     setTypeInfo t arr
addField t@(RVarArray el) = 
    do elInfo <- getTypeInfo el
       arr <- if isStorable elInfo
              then do lst <- vecOf el
                      return $ SerialInfo lst "put" "get" Nothing
              else do lst <- listOf el
                      return $ SerialInfo lst "putList" "getList" Nothing
       setTypeInfo t arr
addField t@(RFixedArray n RBool) =
  do lst <- vecOf RBool
     let arr = SerialInfo lst putWordFromBool getBoolFromWord
                          (Just . B.pack $ show n)
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
addField t@(RUserType n) = do userFlat <- isStorable . fst <$> getMsg n
                              t' <- ros2Hask t
                              setTypeInfo t $ (if userFlat
                                               then defaultFlat
                                               else defaultNonFlat) t'
addField t = ros2Hask t >>= setTypeInfo t . defaultFlat

-- Generate the name of the Haskell type that corresponds to a flat
-- (i.e. non-array) ROS type.
mkFlatType :: MsgType -> ByteString
mkFlatType RBool         = "P.Bool"
mkFlatType RInt8         = "Int.Int8"
mkFlatType RUInt8        = "Word.Word8"
mkFlatType RByte         = "Word.Word8"
mkFlatType RChar         = "Int.Int8"
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
