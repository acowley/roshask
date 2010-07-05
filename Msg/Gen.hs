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

generateMsgType :: ByteString -> Msg -> ByteString
generateMsgType pkgPath msg@(Msg name _ fields) = 
    B.concat [modLine, "\n", imports, dataLine, fieldSpecs, " }\n\n",
              genBinaryInstance msg, "\n\n", 
              genBinaryIterInstance msg, "\n\n",
              genHasHeader msg]
    where tName = pack $ toUpper (head name) : tail name
          modLine = B.concat ["module ", pkgPath, tName, " where"]
          imports = B.concat ["import qualified Prelude as P\n",
                              "import Control.Applicative\n",
                              "import Data.Monoid\n",
                              "import Ros.BinaryIter\n",
                              "import Ros.RosBinary\n",
                              genImports (map snd fields)]
          dataLine = B.concat ["\ndata ", tName, " = ", tName, " { "]
          fieldIndent = B.replicate (B.length dataLine - 3) ' '
          lineSep = B.concat ["\n", fieldIndent, ", "]
          fieldSpecs = B.intercalate lineSep $ map generateField fields

genHasHeader :: Msg -> ByteString
genHasHeader (Msg name _ ((hn, RUserType t):_)) 
    | t == "Header" = B.concat["instance HasHeader ", pack name, 
                               " where\n  getHeader = header\n",
                               "  setSequence x seq = x { ", hn, 
                               " = (", hn, " x) { Header.seq = seq } }\n"]
    | otherwise = ""
genHasHeader _ = ""

generateField :: (ByteString, MsgType) -> ByteString
generateField (name, t) = B.concat [name, " :: ", ros2Hask t]

genImports :: [MsgType] -> ByteString
genImports fieldTypes = B.concat (concatMap (\i -> ["import ", i, "\n"])
                                            (S.toList (allImports fieldTypes)))
     where allImports = foldl' ((. typeDependency) . flip S.union) S.empty

genBinaryInstance :: Msg -> ByteString
genBinaryInstance (Msg name _ fields) = 
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

intImport = singleton "qualified Data.Int as Int"
wordImport = singleton "qualified Data.Word as Word"

typeDependency :: MsgType -> Set ByteString
typeDependency RInt8             = intImport
typeDependency RUInt8            = wordImport
typeDependency RInt16            = intImport
typeDependency RUInt16           = wordImport
typeDependency RUInt32           = wordImport
typeDependency RTime             = singleton "Ros.RosTypes"
typeDependency RDuration         = singleton "Ros.RosTypes"
typeDependency (RFixedArray _ t) = S.union vectorDeps $
                                   typeDependency t
typeDependency (RVarArray t)     = S.union vectorDeps $
                                   typeDependency t
typeDependency (RUserType ut)    = path2Module ut
typeDependency _                 = S.empty

-- Non built-in types are, by default, in the Ros.Std_msgs
-- namespace. If a package path is given, then it is converted to a
-- Haskell hierarchical module name and prefixed by "Ros.".
path2Module :: ByteString -> Set ByteString
path2Module "Header" = S.fromList ["qualified Ros.Std_msgs.Header as Header", 
                                   "Msg.HeaderSupport"]
path2Module p = singleton $
                if B.elem '/' p
                then B.concat ["qualified Ros.",
                               B.intercalate "." . map cap . B.split '/' $ p,
                               " as ", p]
                else B.concat ["qualified Ros.Std_msgs.", p, " as ", p]
    where cap s = B.cons (toUpper (B.head s)) (B.tail s)

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
ros2Hask (RFixedArray _ t) = B.append "V.Vector " (ros2Hask t)
ros2Hask (RVarArray t)     = B.append "V.Vector " (ros2Hask t)
ros2Hask (RUserType t)     = qualify . pack . takeFileName . unpack $ t
    where qualify b = B.concat [b, ".", b]

genBinaryIterInstance :: Msg -> ByteString
genBinaryIterInstance (Msg name _ fields) = 
    B.concat ["instance BinaryIter ", pack name, " where\n",
              "  consume = case ", pack name, " <$> ", 
              B.intercalate " <*> " (replicate (length fields) "consume'"),
              " of\n",
              "              Emit v r1 -> \\r2 -> Emit v (r1 `mappend` r2)\n",
              "              More k -> k"]
                
