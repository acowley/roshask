-- |Generate Haskell source files for ROS .msg types.
{-# LANGUAGE OverloadedStrings #-}
module Gen (generateMsgType) where
import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Analysis (MsgInfo, SerialInfo(..), withMsg, getTypeInfo)
import Types
import FieldImports
import Instances.Binary
import Instances.Storable
import MD5
import Ros.Internal.PathUtil (cap)

generateMsgType :: ByteString -> [ByteString] -> Msg -> MsgInfo ByteString
generateMsgType pkgPath pkgMsgs msg =
  do (fDecls, binInst, st, cons) <- withMsg msg $
                                    (,,,) <$> mapM generateField (fields msg)
                                          <*> genBinaryInstance msg
                                          <*> genStorableInstance msg
                                          <*> genConstants msg
     let fieldSpecs = B.intercalate lineSep fDecls
         (storableImport, storableInstance) = st
     --msgHash <- liftIO $ genHasHash msg
     msgHash <- genHasHash msg
     return $ B.concat [ modLine, "\n"
                       , imports
                       , storableImport
                       , if null fDecls
                         then dataSingleton
                         else B.concat [ dataLine
                                       , fieldSpecs
                                       , "\n"
                                       , fieldIndent
                                       , "} deriving (P.Show, P.Eq, P.Ord, T.Typeable)\n\n"]
                       , binInst, "\n\n"
                       , storableInstance
                       --, genNFDataInstance msg
                       , genHasHeader msg
                       , msgHash
                       , cons ]
    where name = shortName msg
          tName = pack $ toUpper (head name) : tail name
          modLine = B.concat ["{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}\n",
                              "module ", pkgPath, tName, " where"]
          imports = B.concat ["import qualified Prelude as P\n",
                              "import Prelude ((.), (+), (*))\n",
                              "import qualified Data.Typeable as T\n",
                              "import Control.Applicative\n",
                              "import Ros.Internal.RosBinary\n",
                              "import Ros.Internal.Msg.MsgInfo\n",
                              genImports pkgPath pkgMsgs 
                                         (map fieldType (fields msg))]
                              --nfImport]
          dataLine = B.concat ["\ndata ", tName, " = ", tName, " { "]
          dataSingleton = B.concat ["\ndata ", tName, " = ", tName, 
                                    " deriving (P.Show, P.Eq, P.Ord)\n\n"]
          fieldIndent = B.replicate (B.length dataLine - 3) ' '
          lineSep = B.concat ["\n", fieldIndent, ", "]

genHasHeader :: Msg -> ByteString
genHasHeader m = 
    if hasHeader m
    then let hn = fieldName (head (fields m)) -- The header field name
         in B.concat ["instance HasHeader ", pack (cap . shortName $ m), " where\n",
                      "  getSequence = Header.seq . ", hn, "\n",
                      "  getFrame = Header.frame_id . ", hn, "\n",
                      "  getStamp = Header.stamp . " , hn, "\n",
                      "  setSequence seq x' = x' { ", hn, 
                      " = (", hn, " x') { Header.seq = seq } }\n\n"]
    else ""

genHasHash :: Msg -> MsgInfo ByteString
genHasHash m = msgMD5 m >>= return . aux
  where aux md5 = B.concat ["instance MsgInfo ", pack (cap . shortName $ m),
                            " where\n  sourceMD5 _ = \"", pack md5,
                            "\"\n  msgTypeName _ = \"", pack (longName m),
                            "\"\n"]

generateField :: MsgField -> MsgInfo ByteString
generateField (MsgField name t _) = do t' <- hType <$> getTypeInfo t
                                       return $ B.concat [name, " :: ", t']

genConstants :: Msg -> MsgInfo ByteString
genConstants = fmap B.concat . mapM buildLine . constants
    where buildLine (MsgConst name rosType val _) = 
              do t <- hType <$> getTypeInfo rosType
                 return $ B.concat ["\n",name, " :: ", t, "\n", 
                                    name, " = ", val, "\n"]
