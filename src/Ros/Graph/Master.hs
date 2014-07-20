-- Client functionality for the ROS Master API
module Ros.Graph.Master where
import Network.XmlRpc.Client
import Ros.Internal.RosTypes

-- |Subscribe the caller to the specified topic. In addition to
-- receiving a list of current publishers, the subscriber will also
-- receive notifications of new publishers via the publisherUpdate
-- API. Takes the URI of the master, the caller_id, the topic name,
-- the topic type (must be a package-resource name, i.e. the .msg
-- name), and the API URI of the subscriber to register (used for new
-- publisher notifications). Returns a list of XML-RPC API URIs for
-- nodes currently publishing the specified topic.
registerSubscriber :: URI -> String -> TopicName -> TopicType -> String -> 
                      IO (Int, String, [String])
registerSubscriber = flip remote "registerSubscriber"

-- |Unregister the caller as a subscriber of the topic. Takes the URI
-- of the master, the caller_id, the topic name, and the API URI of
-- the subscriber to unregister. Returns zero if the caller was not
-- registered as a subscriber.
unregisterSubscriber :: URI -> String -> TopicName -> String -> 
                        IO (Int, String, Int)
unregisterSubscriber = flip remote "unregisterSubscriber"

-- |Register the caller as a publisher the topic. Takes the URI of the
-- master, the caller_id, the topic name, the topic type (must be a
-- package-resource name, i.e. the .msg name), and the API URI of the
-- publisher to register. Returns a list of the XML-RPC URIs of
-- current subscribers of the topic.
registerPublisher :: URI -> String -> TopicName -> TopicType -> String -> 
                     IO (Int, String, [String])
registerPublisher = flip remote "registerPublisher"

-- |Unregister the caller as a publisher of the topic. Takes the URI of
-- the master, caller_id, the topic name, and the API URI of the
-- publisher to unregister. Returns zero if the caller was not
-- registered as a publisher.
unregisterPublisher :: URI -> String -> TopicName -> String -> 
                       IO (Int, String, Int)
unregisterPublisher = flip remote "unregisterPublisher"


{-
lookupService(caller_id, service)

Lookup all provider of a particular service.

Parameters
caller_id (str)
ROS caller ID
service (str)
Fully-qualified name of service
Returns (int, str, str)
(code, statusMessage, serviceUrl)

service URL is provides address and port of the service. Fails if there is no provider.
-}

lookupService :: URI -> String -> ServiceName -> IO(Int, String, String)
lookupService = flip remote "lookupService"

{-
registerService(caller_id, service, service_api, caller_api)

Register the caller as a provider of the specified service.

Parameters
caller_id (str)
ROS caller ID
service (str)
Fully-qualified name of service
service_api (str)
ROSRPC Service URI
caller_api (str)
XML-RPC URI of caller node
Returns (int, str, int)
(code, statusMessage, ignore)
-}
registerService :: URI -> String -> ServiceName -> URI -> URI -> IO(Int, String, Int)
registerService = flip remote "registerService"

{-
unregisterService(caller_id, service, service_api)

Unregister the caller as a provider of the specified service.

Parameters
caller_id (str)
ROS caller ID
service (str)
Fully-qualified name of service
service_api (str)
API URI of service to unregister. Unregistration will only occur if current registration matches.

Returns (int, str, int)
(code, statusMessage, numUnregistered).

Number of unregistrations (either 0 or 1). If this is zero it means that the caller was not registered as a service provider. The call still succeeds as the intended final state is reached.
-}
unregisterService :: URI -> String -> ServiceName -> URI -> IO(Int, String, Int)
unregisterService = flip remote "unregisterService"
