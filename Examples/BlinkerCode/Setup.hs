import Distribution.Simple
import Ros.Build.SetupUtil

main = defaultMainWithHooks $
       simpleUserHooks { confHook = rosConf, buildHook = rosBuild }
