-- |Generate Haskell source files for ROS .msg types.
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

generateMsgType :: Msg -> String
generateMsgType msg@(Msg name fields) = 
    modLine++"\n"++imports++"\n"++dataLine++fieldSpecs++" }\n\n"++
    genBinaryInstance msg++"\n\n"++
    genBinaryIterInstance msg ++"\n"
    where tName = toUpper (head name) : tail name
          modLine = printf "module %s where" tName
          imports = "import Control.Applicative\n"++
                    "import Data.Binary\n"++
                    "import Data.Monoid\n"++
                    "import BinaryIter\n" ++
                    "import RosBinary\n" ++ 
                    genImports (map snd fields)
          dataLine = printf "data %s = %s { " tName tName
          fieldIndent = replicate (length dataLine - 2) ' '
          lineSep = "\n" ++ fieldIndent ++ ", "
          fieldSpecs = intercalate lineSep $ map generateField fields

generateField :: (ByteString, MsgType) -> String
generateField (name, t) = unpack name ++ " :: " ++ ros2Hask t

genImports :: [MsgType] -> String
genImports fieldTypes = concatMap (printf "import %s\n") 
                                  (S.toList (allImports fieldTypes))
    where allImports = foldl' ((. typeDependency) . flip S.union) S.empty

genBinaryInstance :: Msg -> String
genBinaryInstance (Msg name fields) = 
    printf "instance Binary %s where\n" name ++
    printf "  put x = do " ++
    intercalate ("\n"++replicate 13 ' ') (map putField fields) ++
    printf "\n  get = do " ++
    concatMap (++"\n"++replicate 11 ' ') (map getField fields) ++
    "return $ "++name++" "++
    concatMap ((++" ").unpack.fst) fields 

putField :: (ByteString, MsgType) -> String
putField (name, t) = printf "%s (%s x)" (serialize t) (unpack name)

serialize :: MsgType -> String
serialize (RFixedArray _ t) = printf "V.mapM_ %s" (serialize t)
serialize (RVarArray t)     = 
    printf "(\\a -> putInt32 (V.length a) >> V.mapM_ %s a)" (serialize t)
serialize (RUserType _)     = "put"
serialize t = "put" ++ tail (show t)

getField :: (ByteString, MsgType) -> String
getField (name, t) = printf "%s <- %s" (unpack name) (deserialize t)

deserialize :: MsgType -> String
deserialize (RFixedArray n t) = "getFixed " ++ show n
deserialize (RVarArray t) = "getVarArray"
deserialize (RUserType _) = "get"
deserialize t = "get" ++ tail (show t)

vectorDeps = S.fromList [ "qualified Data.Vector.Unboxed as V" ]

typeDependency :: MsgType -> Set String
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
typeDependency (RUserType ut)    = singleton (unpack (path2Module ut))
typeDependency _                 = S.empty

path2Module :: ByteString -> ByteString
path2Module = B.map (\c -> if c == '/' then '.' else c)

ros2Hask :: MsgType -> String
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
ros2Hask (RFixedArray _ t) = "V.Vector "++ros2Hask t
ros2Hask (RVarArray t)     = "V.Vector "++ros2Hask t
ros2Hask (RUserType t)     = takeFileName $ unpack t

genBinaryIterInstance :: Msg -> String
genBinaryIterInstance (Msg name fields) = 
    printf "instance BinaryIter %s where\n" name ++
    printf "  consume = case %s <$> " name ++
    intercalate " <*> " (replicate (length fields) "consume'") ++ " of\n"++
    printf "              Emit v r1 -> \\r2 -> Emit v (r1 `mappend` r2)\n"++
    printf "              More k -> k"
                
