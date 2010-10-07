-- |Generate Haskell source files for ROS .msg types. Currently only
-- supports arrays of built-in types.
{-# LANGUAGE OverloadedStrings #-}
module Msg.Gen (generateMsgType) where
import Data.ByteString.Char8 (pack, unpack, ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Set (Set, singleton)
import qualified Data.Set as S
import Data.List (intercalate, foldl')
import System.FilePath (takeFileName)
import Text.Printf (printf)
import Ros.Build.DepFinder (findMessage)
import Msg.Analysis (isMsgFlat)
import Msg.Parse (parseMsg)
import Msg.Types

generateMsgType :: ByteString -> [ByteString] -> Msg -> IO ByteString
generateMsgType pkgPath pkgMsgs msg@(Msg name longName md5 fields) =
  do fieldDecls <- mapM (generateField homePkg) fields
     let fieldSpecs = B.intercalate lineSep fieldDecls
     (storableImport, storableInstance) <- genStorableInstance msg
     return $ B.concat [ modLine, "\n"
                       , imports
                       , storableImport
                       , dataLine, fieldSpecs, " } deriving P.Show\n\n"
                       , genBinaryInstance msg, "\n\n"
                       , storableInstance
                       , genHasHeader msg
                       , genHasHash msg ]
    where tName = pack $ toUpper (head name) : tail name
          modLine = B.concat ["{-# LANGUAGE OverloadedStrings #-}\n",
                              "module ", pkgPath, tName, " where"]
          imports = B.concat ["import qualified Prelude as P\n",
                              "import Prelude ((.), (+))\n",
                              "import Control.Applicative\n",
                              "import Ros.RosBinary\n",
                              "import Msg.MsgInfo\n",
                              genImports pkgPath pkgMsgs (map snd fields)]
          dataLine = B.concat ["\ndata ", tName, " = ", tName, " { "]
          fieldIndent = B.replicate (B.length dataLine - 3) ' '
          lineSep = B.concat ["\n", fieldIndent, ", "]
          homePkg = takeWhile (/= '/') longName

hasHeader :: Msg -> Bool
hasHeader (Msg _ _ _ ((_, RUserType "Header"):_)) = True
hasHeader _                                       = False

genHasHeader :: Msg -> ByteString
genHasHeader m@(Msg name _ _ fields) = 
    if hasHeader m
    then let hn = fst (head fields) -- The header field name
         in B.concat ["instance HasHeader ", pack name, " where\n",
                      "  getSequence = Header.seq . ", hn, "\n",
                      "  getFrame = Header.frame_id . ", hn, "\n",
                      "  getStamp = Header.stamp . " , hn, "\n",
                      "  setSequence seq x' = x' { ", hn, 
                      " = (", hn, " x') { Header.seq = seq } }\n\n"]
    else ""

genHasHash :: Msg -> ByteString
genHasHash (Msg sname lname md5 _) = 
    B.concat ["instance MsgInfo ", pack sname,
              " where\n  sourceMD5 _ = \"", pack md5,
              "\"\n  msgTypeName _ = \"", pack lname,
              "\"\n"]

generateField :: String -> (ByteString, MsgType) -> IO ByteString
generateField homePkg (name, t) = do t' <- ros2Hask homePkg t
                                     return $ B.concat [name, " :: ", t']

genImports :: ByteString -> [ByteString] -> [MsgType] -> ByteString
genImports pkgPath pkgMsgs fieldTypes = 
    B.concat $ concatMap (\i -> ["import ", i, "\n"])
                         (S.toList (allImports fieldTypes))
    where getDeps = typeDependency pkgPath pkgMsgs
          allImports = foldl' ((. getDeps) . flip S.union) S.empty

genBinaryInstance :: Msg -> ByteString
genBinaryInstance m@(Msg name _ _ fields) = 
   B.concat ["instance RosBinary ", pack name, " where\n",
             "  put obj' = ", 
             B.intercalate " *> " $ 
              map (\(f,t) -> B.concat [serialize t, " (", f, " obj')"]) fields,
             "\n  get = ", pack name, " <$> ",
             B.intercalate " <*> " (map getField fields),
             if hasHeader m then putMsgHeader else ""]

genStorableInstance :: Msg -> IO (ByteString, ByteString)
genStorableInstance m@(Msg name _ _ fields) = isMsgFlat m >>= return . aux
    where aux False = ("", "")
          aux True = (smImp, stInst)
          peekFields = map (const "SM.peek") fields
          pokeFields = map (\(n,_) -> B.concat ["SM.poke (", n, " obj')"]) fields
          stInst = B.concat ["instance Storable ", pack name, " where\n",
                             "  sizeOf _ = ", totalSize m,"\n",
                             "  alignment _ = alignment nullPtr\n",
                             "  peek = SM.runStorable (", pack name, " <$> ",
                               B.intercalate " *> " peekFields, ")\n",
                             "  poke ptr' obj' = SM.runStorable store' ptr'\n",
                             "    where store' = ",
                               B.intercalate " *> " pokeFields,"\n\n"]
          smImp = B.concat [ "import Foreign.Storable (Storable(..))\n"
                           , "import qualified Ros.Util.StorableMonad as SM\n"
                           , "import Foreign.Ptr (nullPtr)\n" ]

totalSize :: Msg -> ByteString
totalSize (Msg _ _ _ fields) = B.intercalate sep $ map aux fields
    where aux (_,t) = B.concat ["sizeOf (P.undefined::", mkFlatType t, ")"]
          sep = B.append " +\n" $ B.replicate 13 ' '

putMsgHeader :: ByteString
putMsgHeader = "\n  putMsg = putStampedMsg"

putField :: (ByteString, MsgType) -> ByteString
putField (name, t) = B.concat [serialize t, " (", name, " x')"]

serialize :: MsgType -> ByteString
serialize (RFixedArray _ (RUserType _)) = "putFixedList"
serialize (RFixedArray _ t) = "putFixed"
serialize (RVarArray (RUserType _)) = "putList"
serialize _ = "put"

getField :: (ByteString, MsgType) -> ByteString
getField = deserialize . snd

deserialize :: MsgType -> ByteString
deserialize (RFixedArray n (RUserType _)) = B.append "getFixedList " (pack (show n))
deserialize (RFixedArray n t) = B.append "getFixed " (pack (show n))
deserialize (RVarArray (RUserType _)) = "getList"
deserialize _ = "get"

vectorDeps = S.fromList [ "qualified Data.Vector.Storable as V" ]

intImport = singleton "qualified Data.Int as Int"
wordImport = singleton "qualified Data.Word as Word"

typeDependency :: ByteString -> [ByteString] -> MsgType -> Set ByteString
typeDependency _ _ RInt8             = intImport
typeDependency _ _ RUInt8            = wordImport
typeDependency _ _ RInt16            = intImport
typeDependency _ _ RUInt16           = wordImport
typeDependency _ _ RUInt32           = wordImport
typeDependency _ _ RTime             = singleton "Ros.RosTypes"
typeDependency _ _ RDuration         = singleton "Ros.RosTypes"
typeDependency p m (RFixedArray _ t) = S.union vectorDeps $
                                       typeDependency p m t
typeDependency p m (RVarArray t)     = S.union vectorDeps $
                                       typeDependency p m t
typeDependency p m (RUserType ut)    = if elem ut m
                                       then singleton $ 
                                            B.concat ["qualified ", p, ut, 
                                                      " as ", ut]
                                       else path2Module ut
typeDependency _ _ _                 = S.empty

-- Non built-in types are either in the specified package or in the
-- Ros.Std_msgs namespace. If a package path is given, then it is
-- converted to a Haskell hierarchical module name and prefixed by
-- "Ros.".
path2Module :: ByteString -> Set ByteString
path2Module "Header" = S.fromList ["qualified Ros.Roslib.Header as Header", 
                                   "Msg.HeaderSupport"]
path2Module p = singleton $
                if B.elem '/' p
                then B.concat ["qualified Ros.",
                               B.intercalate "." . map cap $ parts,
                               " as ", last parts]
                else B.concat ["qualified Ros.Std_msgs.", p, " as ", p]
    where cap s = B.cons (toUpper (B.head s)) (B.tail s)
          parts = B.split '/' p

-- Given a home package name and a ROS 'MsgType', generate a Haskell
-- type name.
ros2Hask :: String -> MsgType -> IO ByteString
ros2Hask pkg (RFixedArray _ t) = mkArrayType pkg t
ros2Hask pkg (RVarArray t)     = mkArrayType pkg t
ros2Hask pkg RString           = return "P.String"
ros2Hask _   t                 = return $ mkFlatType t

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

-- Make an array type declaration. If the element type of the
-- collection is flat (i.e. does not itself have a field of type
-- 'RVarArray'), then it will have a 'Storable' instance and can be
-- stored in a 'Vector'.
mkArrayType :: String -> MsgType -> IO ByteString
mkArrayType pkg ut@(RUserType t) = 
    findMessage pkg (unpack t) >>= 
    maybe (error $ "Couldn't find definition for " ++ show t) parseMsg >>= 
    either error isMsgFlat >>= 
    toArr
    where toArr True  = return . B.append "V.Vector " . mkFlatType $ ut
          toArr False = return $ B.concat ["[", mkFlatType ut, "]"]
mkArrayType pkg (RFixedArray _ t) = 
    do t'<- ros2Hask pkg t
       return $ B.concat ["[", t', "]"]
mkArrayType pkg (RVarArray t) =
    do t' <- ros2Hask pkg t
       return $ B.concat ["[", t', "]"]
mkArrayType pkg RString = return "[P.String]"
mkArrayType _ t       = return . B.append "V.Vector " $ mkFlatType t
