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
import Msg.Types

generateMsgType :: ByteString -> [ByteString] -> Msg -> ByteString
generateMsgType pkgPath pkgMsgs msg@(Msg name _ md5 fields) = 
    B.concat [modLine, "\n", imports, dataLine, fieldSpecs, 
              " } deriving P.Show\n\n",
              genBinaryInstance msg, "\n\n", 
              --genBinaryIterInstance msg, "\n\n",
              genHasHeader msg,
              genHasHash msg]
    where tName = pack $ toUpper (head name) : tail name
          modLine = B.concat ["{-# LANGUAGE OverloadedStrings #-}\n",
                              "module ", pkgPath, tName, " where"]
          imports = B.concat ["import qualified Prelude as P\n",
                              "import Prelude ((.))\n",
                              "import Control.Applicative\n",
                              "import Data.Monoid\n",
                              "import Data.Typeable\n",
                              "import Ros.BinaryIter\n",
                              "import Ros.RosBinary\n",
                              "import Msg.MsgInfo\n",
                              genImports pkgPath pkgMsgs (map snd fields)]
          dataLine = B.concat ["\ndata ", tName, " = ", tName, " { "]
          fieldIndent = B.replicate (B.length dataLine - 3) ' '
          lineSep = B.concat ["\n", fieldIndent, ", "]
          fieldSpecs = B.intercalate lineSep $ map generateField fields

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

generateField :: (ByteString, MsgType) -> ByteString
generateField (name, t) = B.concat [name, " :: ", ros2Hask t]

genImports :: ByteString -> [ByteString] -> [MsgType] -> ByteString
genImports pkgPath pkgMsgs fieldTypes = 
    B.concat $ concatMap (\i -> ["import ", i, "\n"])
                         (S.toList (allImports fieldTypes))
    where getDeps = typeDependency pkgPath pkgMsgs
          allImports = foldl' ((. getDeps) . flip S.union) S.empty

genBinaryInstance :: Msg -> ByteString
genBinaryInstance m@(Msg name _ _ fields) = 
   B.concat ["instance RosBinary ", pack name, " where\n",
             "  put x' = do ", 
             B.intercalate (B.append "\n" (pack (replicate 14 ' ')))
                           (map putField fields),
             "\n  get = ", pack name, " <$> ",
             B.intercalate " <*> " (map getField fields),
             if hasHeader m then putMsgHeader else ""]

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

-- Non built-in types are either in the current package or in the
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

ros2Hask :: MsgType -> ByteString
ros2Hask RBool             = "P.Bool"
ros2Hask RInt8             = "Int.Int8"
ros2Hask RUInt8            = "Word.Word8"
ros2Hask RInt16            = "Int.Int16"
ros2Hask RUInt16           = "Word.Word16"
ros2Hask RInt32            = "P.Int"
ros2Hask RUInt32           = "Word.Word32"
ros2Hask RInt64            = "Int.Int64"
ros2Hask RUInt64           = "Word.Word64"
ros2Hask RFloat32          = "P.Float"
ros2Hask RFloat64          = "P.Double"
ros2Hask RString           = "P.String"
ros2Hask RTime             = "ROSTime"
ros2Hask RDuration         = "ROSDuration"
ros2Hask (RFixedArray _ t) = case t of
                               RUserType _ -> B.concat ["[", ros2Hask t, "]"]
                               _ -> B.append "V.Vector " (ros2Hask t)
ros2Hask (RVarArray t)     = case t of
                               RUserType _ -> B.concat ["[", ros2Hask t, "]"]
                               _ -> B.append "V.Vector " (ros2Hask t)
ros2Hask (RUserType t)     = qualify . pack . takeFileName . unpack $ t
    where qualify b = B.concat [b, ".", b]

genBinaryIterInstance :: Msg -> ByteString
genBinaryIterInstance (Msg name _ _ fields) = 
    B.concat ["instance BinaryIter ", pack name, " where\n",
              "  consume = case ", pack name, " <$> ", 
              B.intercalate " <*> " (replicate (length fields) "consume'"),
              " of\n",
              "              Emit v r1 -> \\r2 -> Emit v (r1 `mappend` r2)\n",
              "              More k -> k"]
                
