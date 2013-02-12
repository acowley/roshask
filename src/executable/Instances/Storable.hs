{-# LANGUAGE OverloadedStrings #-}
-- |Generate a Storable instance for ROS msg types.
module Instances.Storable (genStorableInstance) where
import Control.Monad ((>=>))
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Types
import Analysis

rosPoke :: MsgType -> ByteString
rosPoke (RFixedArray _ t) = B.concat [ "V.mapM_ (", rosPoke t, ")" ]
rosPoke _ = "SM.poke"

rosPeek :: MsgType -> ByteString
rosPeek (RFixedArray n t) = B.concat [ "V.replicateM ", B.pack (show n), 
                                       " (", rosPeek t, ")" ]
rosPeek _ = "SM.peek"

genStorableInstance :: Msg -> MsgInfo (ByteString, ByteString)
genStorableInstance msg 
  | null (fields msg) = return ("import Foreign.Storable (Storable(..))\n",
                                singletonStorable)
  | otherwise = isFlat msg >>= aux
    where aux False = return ("", "")
          aux True = do sz <- totalSize msg
                        return (smImp, stInst sz)
          --peekFields = map (const "SM.peek") (fields msg)
          peekFields = map (rosPeek . fieldType) (fields msg)
          -- pokeFields = map ((\n -> B.concat ["SM.poke (", n, " obj')"]) . 
          --                   fieldName)
          --                  (fields msg)
          pokeFields = map (\f -> B.concat [ rosPoke (fieldType f)
                                           , " (", fieldName f
                                           , " obj')"])
                           (fields msg)
          name = pack (shortName msg)
          stInst sz = B.concat ["instance Storable ", name, " where\n",
                                "  sizeOf _ = ", sz,"\n",
                                "  alignment _ = 8\n",
                                "  peek = SM.runStorable (", name, " <$> ",
                                B.intercalate " <*> " peekFields, ")\n",
                                "  poke ptr' obj' = ",
                                "SM.runStorable store' ptr'\n",
                                "    where store' = ",
                                B.intercalate " *> " pokeFields,"\n\n"]
          smImp = B.concat [ "import Foreign.Storable (Storable(..))\n"
                           , "import qualified Ros.Internal.Util.StorableMonad"
                           , " as SM\n" ]
          singletonStorable = B.concat [ "instance Storable ", name, " where\n"
                                       , "  sizeOf _ = 1\n"
                                       , "  alignment _ = 1\n"
                                       , "  peek _ = pure ", name, "\n"
                                       , "  poke _ _ = pure ()\n\n" ]


totalSize :: Msg -> MsgInfo ByteString
totalSize msg = B.intercalate sep `fmap` mapM (aux . fieldType) (fields msg)
    where aux = getTypeInfo >=> return . fromJust . size
          sep = B.append " +\n" $ B.replicate 13 ' '

