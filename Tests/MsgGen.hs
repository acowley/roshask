{-# LANGUAGE OverloadedStrings #-}
module MsgGen where
import Analysis (addMsg)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State (evalStateT)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Gen (generateMsgType)
import MD5 (msgMD5)
import Parse (parseMsg)
import ResolutionTypes (emptyMsgContext, alterPkgMap, MsgInfo)
import Ros.Internal.DepFinder (findMessages)
import System.FilePath ((</>), dropExtension, takeFileName)
import Test.Tasty
import Test.Tasty.HUnit
import Types (Msg)

-- | The location of actionlib message definitions.
actionDir :: FilePath
actionDir = "Tests/actionlib_msgs"

-- | Known-good Haskell modules generated from actionlib message
-- definitions.
goldenDir :: FilePath
goldenDir = "Tests/actionlib_msgs/golden"

-- | Manually add a package and its message definitions to the cache.
cachePkg :: FilePath -> MsgInfo ()
cachePkg dir =
  do msgFiles <- liftIO $ findMessages dir
     let msgNames = map (B.pack . dropExtension . takeFileName) msgFiles
         pkg = (dir, M.empty, M.fromList . zip msgNames $ map Left msgFiles)
         pkgName = B.pack $ takeFileName dir
     alterPkgMap (M.insert pkgName pkg)

-- | Prepare the cache with the std_msgs and rosgraph_msgs packages.
prepMsgGen :: MsgInfo ()
prepMsgGen = do cachePkg "Tests/std_msgs"
                cachePkg "Tests/rosgraph_msgs"

-- | Parse actionlib_msgs message definition files and load golden
-- generated code for reference.
actionResources :: [String] -> IO ([Msg], [B.ByteString])
actionResources msgNames = do
  msgs <- fmap (fmap (either error id)) $ mapM parseMsg msgFiles
  gold <- mapM (B.readFile . (goldenDir </>) . (++".hs")) msgNames
  return (msgs,gold)
  where msgFiles = map (((actionDir</>"msg")</>) . (++".msg")) msgNames

-- | Test the actionlib_msgs package's message definitions.
testActionMsgs :: MsgInfo TestTree
testActionMsgs =
  do (msgs,gold) <- liftIO $ actionResources (map fst msgDefs)
     cachePkg actionDir
     mapM_ addMsg msgs
     md5s <- mapM msgMD5 msgs
     let md5Tests = testGroup "MD5" $
                    zipWith (\(n,m) m' -> testCase n $ m @=? m') msgDefs md5s
     msgGen <- mapM (generateMsgType "Ros.Actionlib_msgs."
                                     (map (B.pack . fst) msgDefs))
                    msgs
     let genTests = testGroup "Generated Haskell" $
                    zipWith3 (\n g g' -> testCase n $ g @=? g') 
                             (map fst msgDefs) gold msgGen
     return $ testGroup "actionlib_msgs" [md5Tests, genTests]
  where -- Message names and the corresponding MD5s computed by rosmsg
        msgDefs = [ ("GoalID", "302881f31927c1df708a2dbab0e80ee8")
                  , ("GoalStatus", "d388f9b87b3c471f784434d671988d4a")
                  , ("GoalStatusArray", "8b2b82f13216d0a8ea88bd3af735e619") ]

-- | Test a message definition that includes string constants.
testStringMsg :: MsgInfo TestTree
testStringMsg =
  do msg <- liftIO $ 
            either error id <$> parseMsg "Tests/test_msgs/msg/StringConst.msg"
     gold <- liftIO $ B.readFile "Tests/test_msgs/golden/StringConst.hs"
     cachePkg "Tests/test_msgs"
     _ <- addMsg msg
     md5' <- msgMD5 msg
     gen <- generateMsgType "Ros.Test_msgs." ["StringConst"] msg
     return $ testGroup "String constants"
       [ testCase "MD5" $ md5 @=? md5'
       , testCase "Generated Haskell" $ gold @=? gen ]
  where md5 = "a8e1a25e612660c2e8d3d161e9e91950" -- computed by rosmsg

-- | Tests for message generation.
tests :: IO TestTree
tests = fmap (testGroup "Message generation") $
        flip evalStateT emptyMsgContext $ do
          prepMsgGen
          sequence [testActionMsgs, testStringMsg]
