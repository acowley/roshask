module SlaveAPI (getSubscriptions, getPublications) where
import Control.Applicative
import Control.Arrow (second)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable (cast)
import ROSTypes

data Slave = Slave { getSubscriptions :: [(TopicName, String)]
                   , getPublications :: [(TopicName, String)]
                   , publisherUpdate :: TopicName -> [URI] -> IO Slave }

mkSlave :: Node -> Slave
mkSlave = Slave <$> getSubs <*> getPubs <*> pubUpdate'

topicType :: ETopic -> String
topicType (ETopic t) = name t

getSubs :: Node -> [(String, String)]
getSubs = map (second topicType) . M.toList . subscriptions

getPubs :: Node -> [(String, String)]
getPubs = map (second topicType) . M.toList . publications

pubUpdate :: Node -> TopicName -> [URI] -> Maybe (IO Node)
pubUpdate n name publishers = 
    do topic <- M.lookup name (subscriptions n) :: Maybe ETopic
       (SubTopic doSub knownPubs) <- cast topic
       let pubs' = filter (flip S.notMember knownPubs) publishers
       case pubs' of
         [] -> Nothing
         _ -> return $ do ts <- mapM doSub pubs'
                          let ts' = ts ++ threads n
                              pubs'' = S.union knownPubs (S.fromList pubs')
                              sub = SubTopic doSub pubs''
                              subs' = M.insert name (ETopic sub) 
                                               (subscriptions n)
                          return $ n { threads = ts'
                                     , subscriptions = subs' }

pubUpdate' :: Node -> TopicName -> [URI] -> IO Slave
pubUpdate' n t pubs = case pubUpdate n t pubs of
                        Just updateNode -> do n' <- updateNode
                                              return (mkSlave n')
                        Nothing -> return $ mkSlave n
