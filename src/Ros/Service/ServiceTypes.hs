module Ros.Service.ServiceTypes (ServiceResponseExcept(..)) where

-- | This type represensts the possible error cases that can occur when a service is called by a client.
-- A NotOkExcept occurs when the server replies to a service request with an error message instead of the normal
-- service message. The NotOkExcept string a string sent from the server. See http://wiki.ros.org/ROS/TCPROS
-- A ResponseReadExcept occurs when the roshask service client has problems recieving either the expected service
-- message or NotOkExcept message.
-- MasterExcept is for problems encountered while communicating with the master
-- ConHeadExcept is for an error with the connection header
-- ConnectExcept is for problems with connecting to the server
-- SendRequestExcept is for problems with sending the request
data ServiceResponseExcept = NotOkExcept String | ResponseReadExcept String | MasterExcept String | ConHeadExcept String | ConnectExcept String | SendRequestExcept String
                          deriving (Show, Eq)

