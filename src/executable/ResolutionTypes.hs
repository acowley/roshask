-- |A monad within which to perform message type resolution.
module ResolutionTypes where
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map, insert, alter)
import qualified Data.Map as M

import Types

type MsgCache = Map ByteString (Either FilePath (SerialInfo, Msg))
type TypeCache = Map MsgType SerialInfo
type PkgCache = (FilePath, TypeCache, MsgCache)

-- Mapping from package names to a tuple of the package's directory
-- and a map from the names of message types defined in that package
-- to parsed 'Msg' values for those message types.
type PackageDefs = Map ByteString (FilePath, TypeCache, MsgCache)

-- The context in which message types are resolved is a pair of a
-- current home package (used to lookup unqualified message type
-- names) and a mapping from previously encountered message types to
-- serial info.
data MsgContext = MsgContext { homePkg   :: ByteString
                             -- ^Current package for unqualified names
                             , msgDefs   :: PackageDefs
                             -- ^Mapping from package name to directory 
                             }

-- Code snippets for use in assembling a Haskell declaration (using
-- the hType field that represents a Haskell type), RosBinary and,
-- optionally, Storable instances for a message type that contains a
-- field of this type.
data SerialInfo = SerialInfo { hType    :: ByteString
                             , putField :: ByteString
                             , getField :: ByteString
                             , size     :: Maybe ByteString } 
                  deriving Show

type MsgInfo = StateT MsgContext IO

-- | An empty 'MsgContext' from which one can begin running a
-- 'MsgInfo'.
emptyMsgContext :: MsgContext
emptyMsgContext = MsgContext B.empty M.empty

setHomePkg :: ByteString -> MsgInfo ()
setHomePkg = modify . aux
    where aux n ctxt = ctxt { homePkg = n }

setTypeInfo :: MsgType -> SerialInfo -> MsgInfo SerialInfo
setTypeInfo mt info = modify aux >> return info
    where aux ctxt = let home = homePkg ctxt
                         defs' = alter (fmap addType) home (msgDefs ctxt)
                     in ctxt { msgDefs = defs' }
          addType (p,tc,mc) = (p, insert mt info tc, mc)

-- Apply the function f to the msgDefs field of the current state.
alterPkgMap :: (PackageDefs -> PackageDefs) -> MsgInfo ()
alterPkgMap f = modify aux
    where aux ctxt = ctxt { msgDefs = f (msgDefs ctxt) }

-- Add a binding from a message type string to a parsed 'Msg' value to
-- a package's existing map.
addParsedMsg :: ByteString -> ByteString -> SerialInfo -> Msg -> MsgInfo ()
addParsedMsg pkg msgType info msg =
  do ctxt <- get
     let defs' = alter (fmap aux) pkg (msgDefs ctxt)
     put $ ctxt { msgDefs = defs' }
  where aux (p, tc, mc) = (p, tc, insert msgType (Right (info, msg)) mc)
