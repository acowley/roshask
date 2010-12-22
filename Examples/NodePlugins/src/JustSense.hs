module Main (main) where
import Ros.Node
import SenseUFO

main = runNode "Sense" senseUFO
