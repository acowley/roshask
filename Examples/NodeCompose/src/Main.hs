module Main (main) where
import Ros.Node
import Telescope
import DetectUFO

main = runNode "NodePlugins" $ telescope >> detectUFO
