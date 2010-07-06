module Ros.RunNode where
import Ros.MasterAPI
import Ros.SlaveAPI

registerNode :: RosSlave s => s -> IO ()
registerNode n = undefined

runNode :: RosSlave s => s -> IO ()
runNode s = registerNode s >> runSlave s 