-- |Generate Haskell source files for ROS .msg types. Currently only
-- supports arrays of built-in types.
{-# LANGUAGE OverloadedStrings #-}
module MsgGen where
import Data.ByteString.Char8 (pack, unpack, ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Set (Set, singleton)
import qualified Data.Set as S
import Data.List (intercalate, foldl')
import System.FilePath (takeFileName)
import Text.Printf (printf)
import MsgTypes

generateMsgType :: Msg -> ByteString
generateMsgType msg@(Msg name fields) = 
    B.concat [modLine, "\n", imports, dataLine, fieldSpecs, " }\n\n",
              genBinaryInstance msg, "\n\n", 
              genBinaryIterInstance msg, "\n"]
    where tName = pack $ toUpper (head name) : tail name
          modLine = B.concat ["module ", tName, " where"]
          imports = B.concat ["import Control.Applicative\n",
                              "import Data.Monoid\n",
                              "import BinaryIter\n",
                              "import RosBinary\n",
                              genImports (map snd fields)]
          dataLine = B.concat ["\ndata ", tName, " = ", tName, " { "]
          fieldIndent = B.replicate (B.length dataLine - 3) ' '
          lineSep = B.concat ["\n", fieldIndent, ", "]
          fieldSpecs = B.intercalate lineSep $ map generateField fields

generateField :: (ByteString, MsgType) -> ByteString
generateField (name, t) = B.concat [name, " :: ", ros2Hask t]

genImports :: [MsgType] -> ByteString
genImports fieldTypes = B.concat (concatMap (\i -> ["import ", i, "\n"])
                                            (S.toList (allImports fieldTypes)))
     where allImports = foldl' ((. typeDependency) . flip S.union) S.empty

genBinaryInstance :: Msg -> ByteString
genBinaryInstance (Msg name fields) = 
   B.concat ["instance BinaryCompact ", pack name, " where\n",
             "  put x = do ", 
             B.intercalate (B.append "\n" (pack (replicate 13 ' ')))
                           (map putField fields),
             "\n  get = ", pack name, " <$> ",
             B.intercalate " <*> " (map getField fields)]

putField :: (ByteString, MsgType) -> ByteString
putField (name, t) = B.concat [serialize t, " (", name, " x)"]

serialize :: MsgType -> ByteString
serialize (RFixedArray _ t) = "putFixed"
serialize _ = "put"

getField :: (ByteString, MsgType) -> ByteString
getField = deserialize . snd

deserialize :: MsgType -> ByteString
deserialize (RFixedArray n t) = B.append "getFixed " (pack (show n))
deserialize _ = "get"

vectorDeps = S.fromList [ "qualified Data.Vector.Unboxed as V" ]

typeDependency :: MsgType -> Set ByteString
typeDependency RInt8             = singleton "Data.Int"
typeDependency RUInt8            = singleton "Data.Word"
typeDependency RInt16            = singleton "Data.Int"
typeDependency RUInt16           = singleton "Data.Word"
typeDependency RUInt32           = singleton "Data.Word"
typeDependency RTime             = singleton "ROSTypes"
typeDependency RDuration         = singleton "ROSTypes"
typeDependency (RFixedArray _ t) = S.union vectorDeps $
                                   typeDependency t
typeDependency (RVarArray t)     = S.union vectorDeps $
                                   typeDependency t
typeDependency (RUserType ut)    = singleton (path2Module ut)
typeDependency _                 = S.empty

path2Module :: ByteString -> ByteString
path2Module = B.map (\c -> if c == '/' then '.' else c)

ros2Hask :: MsgType -> ByteString
ros2Hask RBool             = "Bool"
ros2Hask RInt8             = "Int8"
ros2Hask RUInt8            = "Word8"
ros2Hask RInt16            = "Int16"
ros2Hask RUInt16           = "Word16"
ros2Hask RInt32            = "Int"
ros2Hask RUInt32           = "Word32"
ros2Hask RInt64            = "Int64"
ros2Hask RUInt64           = "Word64"
ros2Hask RFloat32          = "Float"
ros2Hask RFloat64          = "Double"
ros2Hask RString           = "String"
ros2Hask RTime             = "ROSTime"
ros2Hask RDuration         = "ROSDuration"
ros2Hask (RFixedArray _ t) = B.append "V.Vector " (ros2Hask t)
ros2Hask (RVarArray t)     = B.append "V.Vector " (ros2Hask t)
ros2Hask (RUserType t)     = pack $ takeFileName $ unpack t

genBinaryIterInstance :: Msg -> ByteString
genBinaryIterInstance (Msg name fields) = 
    B.concat ["instance BinaryIter ", pack name, " where\n",
              "  consume = case ", pack name, " <$> ", 
              B.intercalate " <*> " (replicate (length fields) "consume'"),
              " of\n",
              "              Emit v r1 -> \\r2 -> Emit v (r1 `mappend` r2)\n",
              "              More k -> k"]
                
