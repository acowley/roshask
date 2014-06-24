{-# LANGUAGE ScopedTypeVariables #-}
-- |Client interface for the ROS Parameter Server API. Note that
-- dictionary values are not supported.
module Ros.Graph.ParameterServer (deleteParam, setParam, getParam, searchParam, 
                                  subscribeParam, unsubscribeParam, hasParam, 
                                  getParamNames) where
import Network.XmlRpc.Client
import Network.XmlRpc.Internals (XmlRpcType)
import Ros.Internal.RosTypes

-- |Delete a parameter on the server.
deleteParam :: URI -> CallerID -> ParamName -> IO (Int,String,Int)
deleteParam = flip remote "deleteParam"

-- |Set a parameter value on the server.
setParam :: XmlRpcType a => URI -> CallerID -> ParamName -> a -> IO (Int,String,Int)
setParam = flip remote "setParam"

-- Helper for extracting a value from a tuple returned by the
-- parameter server.
handleParam :: (Int,String,a) -> IO (Maybe a)
handleParam (1,_,x) = return $ Just x
handleParam (_,_,_) = return Nothing

-- |Retrieve parameter value from server.
getParam :: XmlRpcType a => URI -> CallerID -> ParamName -> IO (Maybe a)
getParam uri caller name = handleParam =<< remote uri "getParam" caller name

-- |Search for a parameter name on the Parameter Server. The search
-- starts in the caller's namespace and proceeds upwards through
-- parent namespaces until the Parameter Server finds a matching
-- key. The first non-trivial partial match is returned.
searchParam :: URI -> CallerID -> ParamName -> IO (Maybe String)
searchParam uri caller name = handle =<< remote uri "searchParam" caller name
    where handle :: (Int, String, String) -> IO (Maybe String)
          handle (1, _, n) = return $ Just n
          handle (_, _, _) = return $ Nothing

-- |Retrieve parameter value from server and subscribe to updates to
-- that param. See paramUpdate() in the 'Ros.SlaveAPI' API.
subscribeParam :: XmlRpcType a => URI -> CallerID -> URI -> ParamName -> 
                  IO (Maybe a)
subscribeParam uri caller myUri name = 
    handleParam =<< remote uri "subscribeParam" caller myUri name

-- |Unsubscribe from updates to a parameter. If there is an error, the
-- status message is returned on the 'Left'; otherwise whether or not
-- the caller was previously subscribed to the parameter is returned
-- on the 'Right'.
unsubscribeParam :: URI -> CallerID -> URI -> ParamName -> IO (Either String Bool)
unsubscribeParam uri caller myUri name = 
    handle =<< remote uri "unsubscribeParam" caller myUri name
    where handle :: (Int, String, Int) -> IO (Either String Bool)
          handle (1, _, 0) = return $ Right False
          handle (1, _, _) = return $ Right True
          handle (_, msg, _) = return $ Left msg

-- |Check if a parameter is stored on the server. If there is an
-- error, the status message is returned on the 'Left'; otherwise the
-- query response is returned on the 'Right'.
hasParam :: URI -> CallerID -> ParamName -> IO (Either String Bool)
hasParam uri caller name = handle =<< remote uri "hasParam" caller name
    where handle :: (Int,String,Bool) -> IO (Either String Bool)
          handle (1,_,b) = return $ Right b
          handle (_,msg,_) = return $ Left msg

-- |Get a list of all parameter names stored on this server.
getParamNames :: URI -> CallerID -> IO (Either String [String])
getParamNames uri caller = handle =<< remote uri "getParamNames" caller
    where handle :: (Int, String, [String]) -> IO (Either String [String])
          handle (1,_,names) = return $ Right names
          handle (_,msg,_) = return $ Left msg
