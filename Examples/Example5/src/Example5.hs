{-# LANGUAGE TemplateHaskell #-}
module Example5 (main) where
import Ros.Node
import Ros.Sensor_msgs.PointCloud
import Ros.Geometry_msgs.Point32
import Ros.Logging

main = do putStrLn "Hello"
          putStrLn . show $ $(logDebug "Here we go")
