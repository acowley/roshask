-- |Generate Haskell source files for ROS .msg types.
{-# LANGUAGE OverloadedStrings #-}
module Ros.Core.Msg.Gen (generateMsgType) where
import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Ros.Core.Msg.Analysis (MsgInfo, SerialInfo(..), withMsg, isFlat, 
                              getTypeInfo, liftIO)
import Ros.Core.Msg.Types
import Ros.Core.Msg.BinaryInstance
import Ros.Core.Msg.FieldImports
import Ros.Core.Msg.StorableInstance

generateMsgType :: ByteString -> [ByteString] -> Msg -> MsgInfo ByteString
generateMsgType pkgPath pkgMsgs msg@(Msg name _ _ fields _) =
  do (fDecls, binInst, st, cons) <- withMsg msg $
                                    (,,,) <$> mapM generateField fields
                                          <*> genBinaryInstance msg
                                          <*> genStorableInstance msg
                                          <*> genConstants msg
     let fieldSpecs = B.intercalate lineSep fDecls
         (storableImport, storableInstance) = st
     msgHash <- liftIO $ genHasHash msg
     return $ B.concat [ modLine, "\n"
                       , imports
                       , storableImport
                       , dataLine, fieldSpecs, " } deriving P.Show\n\n"
                       , binInst, "\n\n"
                       , storableInstance
                       , genHasHeader msg
                       , msgHash
                       , cons ]
    where tName = pack $ toUpper (head name) : tail name
          modLine = B.concat ["{-# LANGUAGE OverloadedStrings #-}\n",
                              "module ", pkgPath, tName, " where"]
          imports = B.concat ["import qualified Prelude as P\n",
                              "import Prelude ((.), (+))\n",
                              "import Control.Applicative\n",
                              "import Ros.Core.RosBinary\n",
                              "import Ros.Core.Msg.MsgInfo\n",
                              genImports pkgPath pkgMsgs (map snd fields)]
          dataLine = B.concat ["\ndata ", tName, " = ", tName, " { "]
          fieldIndent = B.replicate (B.length dataLine - 3) ' '
          lineSep = B.concat ["\n", fieldIndent, ", "]

genHasHeader :: Msg -> ByteString
genHasHeader m@(Msg name _ _ fields _) = 
    if hasHeader m
    then let hn = fst (head fields) -- The header field name
         in B.concat ["instance HasHeader ", pack name, " where\n",
                      "  getSequence = Header.seq . ", hn, "\n",
                      "  getFrame = Header.frame_id . ", hn, "\n",
                      "  getStamp = Header.stamp . " , hn, "\n",
                      "  setSequence seq x' = x' { ", hn, 
                      " = (", hn, " x') { Header.seq = seq } }\n\n"]
    else ""

genHasHash :: Msg -> IO ByteString
genHasHash (Msg sname lname md5 _ _) = 
    md5 >>= \md5 -> return $
        B.concat ["instance MsgInfo ", pack sname,
                  " where\n  sourceMD5 _ = \"", pack md5,
                  "\"\n  msgTypeName _ = \"", pack lname,
                  "\"\n"]

generateField :: (ByteString, MsgType) -> MsgInfo ByteString
generateField (name, t) = do t' <- hType <$> getTypeInfo t
                             return $ B.concat [name, " :: ", t']

genConstants :: Msg -> MsgInfo ByteString
genConstants = fmap B.concat . mapM buildLine . constants
    where buildLine (name, rosType, val) = 
              do t <- hType <$> getTypeInfo rosType
                 return $ B.concat ["\n",name, " :: ", t, "\n", 
                                    name, " = ", val, "\n"]
