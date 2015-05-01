module TopicTest (topicTests) where
import Test.Tasty
import Test.Tasty.HUnit
import Ros.Topic.Util (fromList, toList, leftThenRight)

topicTests :: TestTree
topicTests = testGroup "Topic Tests" [ leftThenRightTest]

leftThenRightTest :: TestTree
leftThenRightTest = testCase "leftThenRight" $ do
  let t = fromList $ zipWith ($) [Left, Right, Right, Left, Left, Right] [1,2..]
  ls <- toList $ leftThenRight t
  ([(1,2),(5,6)] :: [(Int,Int)]) @=? take 2 ls
