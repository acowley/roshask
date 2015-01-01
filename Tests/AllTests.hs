module Main where
import qualified MsgGen
import qualified TopicTest
import Test.Tasty

main :: IO ()
main = do genTests <- MsgGen.tests
          defaultMain $ testGroup "roshask" [
            testGroup "roshask executable"
                      [ genTests ]
            , testGroup "roshask library"
              [ TopicTest.topicTests]]
