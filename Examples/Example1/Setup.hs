import Distribution.Simple
import Ros.Core.Build.SetupUtil

main = defaultMainWithHooks $
       simpleUserHooks { buildHook = rosBuild, confHook = rosConf }

