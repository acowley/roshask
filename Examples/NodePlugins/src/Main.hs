module Main (main) where
import Ros.Node
import SenseUFO
import DetectUFO

main = runNode "NodePlugins" $ senseUFO >> detectUFO
