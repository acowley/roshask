{-# LANGUAGE OverloadedStrings #-}
-- |Generate an NFData instance for ROS msg types.
module Ros.Core.Msg.NFDataInstance where
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Ros.Core.Msg.Types

nfImport :: ByteString
nfImport = "import qualified Control.DeepSeq as D\n"

genNFDataInstance :: Msg -> ByteString
genNFDataInstance msg = 
  B.concat[ "instance D.NFData ", name, " where\n"
          , "  rnf = "
          , B.intercalate " `seqAp` " 
                          (map (\n -> B.append "D.rnf . " n) fieldNames)
          , "\n"
          , "    where seqAp f g = (\\x y -> x `P.seq` y `P.seq` ()) <$> f <*> g\n\n"
          ]
    where name = B.pack (shortName msg)
          fieldNames = map fst (fields msg)
