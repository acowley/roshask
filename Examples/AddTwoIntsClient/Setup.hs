import Distribution.Simple
import Ros.Internal.SetupUtil

main = defaultMainWithHooks $
       simpleUserHooks { confHook = rosConf }
