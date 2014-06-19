{-# LANGUAGE OverloadedStrings #-}
-- | Analyze a MsgType to determine the module imports needed for the
-- message field types.
module FieldImports (genImports) where
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.List (foldl')
import Data.Set (Set, singleton)
import qualified Data.Set as S
import Types

genImports :: ByteString -> [ByteString] -> [MsgType] -> ByteString
genImports pkgPath pkgMsgs fieldTypes = 
    B.concat $ concatMap (\i -> ["import ", i, "\n"])
                         (S.toList (allImports fieldTypes))
    where getDeps = typeDependency pkgPath pkgMsgs
          allImports = foldl' ((. getDeps) . flip S.union) S.empty

intImport, wordImport, vectorDeps :: Set ByteString
intImport = singleton "qualified Data.Int as Int"
wordImport = singleton "qualified Data.Word as Word"
vectorDeps = S.fromList [ "qualified Data.Vector.Storable as V" ]

-- Generate a 'Set' of modules to be imported for the specified
-- 'MsgType'. The first argument is the package of the message type
-- currently being generated, the second argument is a list of all the
-- messages defined in that package (which can be referred to with
-- unqualified names), the third is the 'MsgType' to generate imports
-- for.
typeDependency :: ByteString -> [ByteString] -> MsgType -> Set ByteString
typeDependency _ _ RBool                = wordImport
typeDependency _ _ RInt8                = intImport
typeDependency _ _ RChar                = intImport
typeDependency _ _ RUInt8               = wordImport
typeDependency _ _ RByte                = wordImport
typeDependency _ _ RInt16               = intImport
typeDependency _ _ RUInt16              = wordImport
typeDependency _ _ RUInt32              = wordImport
typeDependency _ _ RUInt64              = wordImport
typeDependency _ _ RInt64               = intImport
typeDependency _ _ RTime                = singleton "Ros.Internal.RosTypes"
typeDependency _ _ RDuration            = singleton "Ros.Internal.RosTypes"
typeDependency p m (RFixedArray _ t)    = S.union vectorDeps $
                                       typeDependency p m t
typeDependency p m (RVarArray t)        = S.union vectorDeps $
                                       typeDependency p m t
typeDependency _ _ (RUserType "Header") = 
    S.fromList ["qualified Ros.Std_msgs.Header as Header", 
                "Ros.Internal.Msg.HeaderSupport"]
typeDependency p m (RUserType ut)       = if elem ut m
                                          then singleton $ 
                                               B.concat ["qualified ", p, ut, 
                                                         " as ", ut]
                                          else path2Module ut
typeDependency _ _ _                    = S.empty

-- Non built-in types are either in the specified package or in the
-- Ros.Std_msgs namespace. If a package path is given, then it is
-- converted to a Haskell hierarchical module name and prefixed by
-- "Ros.".
path2Module :: ByteString -> Set ByteString
path2Module p 
    | B.elem '/' p = singleton $
                     B.concat ["qualified Ros.",
                               B.intercalate "." . map cap $ parts,
                               " as ", last parts]
    | otherwise    = singleton $
                     B.concat ["qualified Ros.Std_msgs.", p, " as ", p]
    where cap s = B.cons (toUpper (B.head s)) (B.tail s)
          parts = B.split '/' p
