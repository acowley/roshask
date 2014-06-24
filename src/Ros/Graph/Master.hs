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



