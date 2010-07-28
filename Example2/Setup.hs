import Distribution.Simple
import Ros.Build.SetupUtil

main = defaultMainWithHooks $
       simpleUserHooks { buildHook = rosBuild, confHook = rosConf }

