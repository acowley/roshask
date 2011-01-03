module Main (main) where
import Ros.Node
import Telescope

main = runNode "Sense" telescope
