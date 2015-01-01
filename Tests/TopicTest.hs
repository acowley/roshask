module TopicTest (topicTests) where
import Test.Tasty
import Test.Tasty.HUnit
import Ros.Topic.Util (fromList, toList, topicRate, firstThenSecond)
import qualified Ros.Topic as Topic

topicTests :: TestTree
topicTests = testGroup "Topic Tests" [ firstThenSecondTest]

firstThenSecondTest :: TestTree
firstThenSecondTest = testCase "firstThenSecond" $ do
  -- Take the tails of the topics so that this test does not depend on
  -- the order of the forkIO calls in Ros.Topic.Util.(<+>)
  let t1 = Topic.tail $ topicRate 1000 $ fromList ([1,2 ..] :: [Int])
      t2 = Topic.tail $ topicRate 100 $ fromList ([1,2..] :: [Int])
  ls <- toList $ firstThenSecond t1 t2
  [(10,2),(20,3),(30,4)] @=? take 3 ls
