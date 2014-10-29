-- |Generate Haskell source files for ROS .msg types.
{-# LANGUAGE OverloadedStrings #-}
module Gen (generateMsgType, generateSrvTypes) where
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

data GenArgs = GenArgs {genExtraImport :: ByteString
                       , genPkgPath :: ByteString
                       , genPkgMsgs :: [ByteString]}

-- | pkgMsgs is a list of all the Mesages defined in the package (can be refered to
-- | with unqualified names)
generateSrvTypes :: ByteString -> [ByteString] -> Srv -> MsgInfo (ByteString, ByteString)
generateSrvTypes pkgPath pkgMsgs srv = do
  let msgs = [srvRequest srv, srvResponse srv]
  srvInfo <- mapM (genSrvInfo srv) msgs
  requestResponseMsgs <-
    mapM (generateMsgTypeExtraImport GenArgs{genExtraImport = "import Ros.Internal.Msg.SrvInfo\n"
                                               , genPkgPath=pkgPath
                                               , genPkgMsgs=pkgMsgs})
    msgs
  let [requestType, responseType] = zipWith B.append requestResponseMsgs srvInfo
  return (requestType, responseType)

generateMsgType :: ByteString -> [ByteString] -> Msg -> MsgInfo ByteString
generateMsgType pkgPath pkgMsgs =
  generateMsgTypeExtraImport GenArgs {genExtraImport=""
                                     , genPkgPath=pkgPath
                                     , genPkgMsgs=pkgMsgs}

generateMsgTypeExtraImport :: GenArgs -> Msg -> MsgInfo ByteString
generateMsgTypeExtraImport (GenArgs {genExtraImport=extraImport, genPkgPath=pkgPath, genPkgMsgs=pkgMsgs}) msg =
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
                                       , "} deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)\n\n"]
                       , binInst, "\n\n"
                       , storableInstance
                       --, genNFDataInstance msg
                       , genHasHeader msg
                       , msgHash
                       , genDefault msg
                       , cons ]
    where name = shortName msg
          tName = pack $ toUpper (head name) : tail name
          modLine = B.concat ["{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}\n",
                              "module ", pkgPath, tName, " where"]
          imports = B.concat ["import qualified Prelude as P\n",
                              "import Prelude ((.), (+), (*))\n",
                              "import qualified Data.Typeable as T\n",
                              "import Control.Applicative\n",
                              "import Ros.Internal.RosBinary\n",
                              "import Ros.Internal.Msg.MsgInfo\n",
                              "import qualified GHC.Generics as G\n",
                              "import qualified Data.Default.Generics as D\n",
                              extraImport,
                              genImports pkgPath pkgMsgs 
                                         (map fieldType (fields msg))]
                              --nfImport]
          dataLine = B.concat ["\ndata ", tName, " = ", tName, " { "]
          dataSingleton = B.concat ["\ndata ", tName, " = ", tName, 
                                    " deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)\n\n"]
          fieldIndent = B.replicate (B.length dataLine - 3) ' '
          lineSep = B.concat ["\n", fieldIndent, ", "]

genHasHeader :: Msg -> ByteString
genHasHeader m = 
    if hasHeader m
    then let hn = fieldName (head (fields m)) -- The header field name
         in B.concat ["instance HasHeader ", pack (shortName m), " where\n",
                      "  getSequence = Header.seq . ", hn, "\n",
                      "  getFrame = Header.frame_id . ", hn, "\n",
                      "  getStamp = Header.stamp . " , hn, "\n",
                      "  setSequence seq x' = x' { ", hn, 
                      " = (", hn, " x') { Header.seq = seq } }\n\n"]
    else ""

genDefault :: Msg -> ByteString
genDefault m = B.concat["instance D.Default ", pack (shortName m), "\n\n"]

genHasHash :: Msg -> MsgInfo ByteString
genHasHash m = fmap aux (msgMD5 m)
  where aux md5 = B.concat ["instance MsgInfo ", pack (shortName m),
                            " where\n  sourceMD5 _ = \"", pack md5,
                            "\"\n  msgTypeName _ = \"", pack (fullRosMsgName m),
                            "\"\n\n"]
genSrvInfo :: Srv -> Msg -> MsgInfo ByteString
genSrvInfo s m = fmap aux (srvMD5 s)
  where aux md5 = B.concat ["instance SrvInfo ", pack (shortName m),
                            " where\n  srvMD5 _ = \"", pack md5,
                            "\"\n  srvTypeName _ = \"", pack (fullRosSrvName s),
                            "\"\n\n"]

generateField :: MsgField -> MsgInfo ByteString
generateField (MsgField name t _) = do t' <- hType <$> getTypeInfo t
                                       return $ B.concat [name, " :: ", t']

genConstants :: Msg -> MsgInfo ByteString
genConstants = fmap B.concat . mapM buildLine . constants
    where escapeQuotes RString x =
            B.concat [ "\""
                     , B.intercalate "\\\"" (B.split '"' x)
                     , "\"" ]
          escapeQuotes _       x = x
          buildLine (MsgConst name rosType val _) = 
              do t <- hType <$> getTypeInfo rosType
                 return $ B.concat [ name, " :: ", t, "\n"
                                   , name, " = "
                                   , escapeQuotes rosType val, "\n\n"]
