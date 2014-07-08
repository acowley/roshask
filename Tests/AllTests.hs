module Main where
import qualified MsgGen
import Test.Tasty

main :: IO ()
main = do genTests <- MsgGen.tests
          defaultMain $
            testGroup "roshask executable"
                      [ genTests ]
