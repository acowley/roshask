module Main (main) where
import Ros.Node
import Telescope (telescope)
import DetectUFO (detectUFO)

main = runNode "NodeCompose" $ telescope >> detectUFO
