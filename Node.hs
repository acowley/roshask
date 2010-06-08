module Node where
import Control.Concurrent.STM.TVar (TVar)
import Data.Set (Set)
import Data.Typeable (Typeable, TypeRep)
import Control.Concurrent (ThreadId)
import ROSTypes

data Subscription = Subscription { knownPubs :: Set URI
                                 , addPub    :: URI -> IO ()
                                 , subType   :: TypeRep
                                 , subStats  :: TVar SubStats }

data Publication = Publication { subscribers :: Set URI
                               , pubType     :: TypeRep
                               , pubStats    :: TVar PubStats
                               , pubThread   :: ThreadId }

mkSub :: Typeable a => (Stream a -> b) -> Subscription
mkSub handler = undefined

mkPub :: Typeable a => Stream a -> Publication
mkPub gen = undefined

runNode :: NodeName -> 
           [(TopicName, Subscription)] -> 
           [(TopicName, Publication)] -> IO ()
runNode name subs pubs = return ()