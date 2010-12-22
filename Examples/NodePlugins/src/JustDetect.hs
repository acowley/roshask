module Main (main) where
import Ros.Node
import DetectUFO

main = runNode "Detect" detectUFO
