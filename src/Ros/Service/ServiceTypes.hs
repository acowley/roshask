module Ros.Service.ServiceTypes (ServiceResponseError(..)) where

import Control.Monad.Error

-- | This type represensts the possible error cases that can occur when a service is called by a client.
-- A NotOkError occurs when the server replies to a service request with an error message instead of the normal
-- service message. The NotOkError string a string sent from the server. See http://wiki.ros.org/ROS/TCPROS
-- A ResponseReadError occurs when the roshask service client has problems recieving either the expected service
-- message or NotOkError message.
-- MasterError is for problems encountered while communicating with the master
-- ConHeadError is for an error with the connection header
-- DefaultError is used by the Error typeclass instance
data ServiceResponseError = NotOkError String | ResponseReadError String | MasterError String | ConHeadError String | DefaultError String
                          deriving (Show, Eq)

instance Error ServiceResponseError where
  noMsg = DefaultError ""
  strMsg x = DefaultError x
