import Distribution.Simple
import Ros.Core.Build.SetupUtil

main = defaultMainWithHooks $
       simpleUserHooks { confHook = rosConf, buildHook = rosBuild }
