import Distribution.Simple
import Ros.Build.SetupUtil

main = defaultMainWithHooks $
       simpleUserHooks { preBuild = addRosMsgPaths (Executables ["RosHask1"]) }
