{-# LANGUAGE OverloadedStrings #-}
module MsgGen where
import Analysis (addMsg)
import Control.Applicative
import Control.Monad (zipWithM)
import Control.Monad.IO.Class
import Control.Monad.State (evalStateT)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Gen (generateMsgType)
import MD5 (msgMD5, srvMD5)
import Parse (parseMsg, parseSrv)
import ResolutionTypes (emptyMsgContext, alterPkgMap, MsgInfo)
import Ros.Internal.DepFinder (findMessages)
import System.FilePath ((</>), dropExtension, takeFileName)
import Test.Tasty
import Test.Tasty.HUnit

-- CONSTANTS

-- | The location of actionlib message definitions.
actionDir :: FilePath
actionDir = "Tests/actionlib_msgs"

-- | The location of actionlib golden Haskell files.
goldenDir :: FilePath
goldenDir = "Tests/actionlib_msgs/golden"

-- HELPER FUNCTIONS

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

--TEST GENERATORS

-- | Test that the generated Haskell code matches a golden Haskell file
-- | The first argument is a path to the test directory
-- | The second argument is a list of paths to the message file
-- | The third argument is a list of paths to the golden Haskell file
-- | Precondition: cachePkg has been called
testGeneratedHaskell :: B.ByteString -> [FilePath] -> [FilePath] -> [B.ByteString] -> MsgInfo TestTree
testGeneratedHaskell packagePath msgPaths haskellPaths moduleNames =
  do msgs <- liftIO $ mapM (fmap (either error id) . parseMsg) msgPaths
     golds <- liftIO $ mapM B.readFile haskellPaths
     mapM_ addMsg msgs
     gens <- mapM (generateMsgType packagePath moduleNames) msgs
     return $ testGroup "Generated Haskell" $ zipWith3
       (\name gold gen -> testCase (B.unpack name) $ gold @=? gen) moduleNames golds gens

-- | Precondition: cachePkg has been called
testMD5 :: FilePath -> String -> MsgInfo TestTree
testMD5 msgPath  md5 =
  do msg <- liftIO $ 
             either error id <$> parseMsg msgPath
     genMD5 <- msgMD5 msg
     return $ testCase ("MD5 for " ++ msgPath) $ md5 @=? genMD5

testSrvMD5 :: FilePath -> String -> MsgInfo TestTree
testSrvMD5 srvPath md5 =
  do srv <- liftIO $ 
             either error id <$> parseSrv srvPath
     genMD5 <- srvMD5 srv
     return $ testCase ("MD5 for " ++ srvPath) $ md5 @=? genMD5

-- TESTS

testActionMsgs :: MsgInfo TestTree
testActionMsgs =
  do cachePkg actionDir
     md5TestList <- zipWithM testMD5 msgFiles md5s
     let md5Tests = testGroup "MD5s" md5TestList
     genTests <- testGeneratedHaskell "Ros.Actionlib_msgs." msgFiles haskellFiles (map B.pack msgNames)
     return $ testGroup "actionlib_msgs" [md5Tests, genTests]
  where -- Message names and the corresponding MD5s computed by rosmsg
    msgNames = map fst msgDefs
    md5s = map snd msgDefs
    msgFiles = map (((actionDir</>"msg")</>) . (++".msg")) msgNames
    haskellFiles =  map ((goldenDir </>) . (++".hs")) msgNames
    msgDefs = [ ("GoalID", "302881f31927c1df708a2dbab0e80ee8")
              , ("GoalStatus", "d388f9b87b3c471f784434d671988d4a")
              , ("GoalStatusArray", "8b2b82f13216d0a8ea88bd3af735e619") ]

-- | Test a message definition that includes string constants.
testStringMsg :: MsgInfo TestTree
testStringMsg = do
  let testDir = "Tests/test_msgs"
      messageFile = "Tests/test_msgs/msg/StringConst.msg"
      haskellFile = "Tests/test_msgs/golden/StringConst.hs"
  cachePkg testDir
  gen <- testGeneratedHaskell "Ros.Test_msgs." [messageFile] [haskellFile] ["StringConsts"]
  md5 <- testMD5 messageFile "a8e1a25e612660c2e8d3d161e9e91950"
  return $ testGroup "String constants" [gen, md5]

--Test that the MD5 for a service is generated correctly
addTwoIntsServiceMD5 :: MsgInfo TestTree
addTwoIntsServiceMD5 = do
  md5Test <- testSrvMD5 file md5
  return $ testGroup "Services" [md5Test]
  where
    file = "Tests/test_srvs/srv/AddTwoInts.srv"
    md5 = "6a2e34150c00229791cc89ff309fff21"

-- | ROOT TEST

-- | Tests for message generation.
tests :: IO TestTree
tests =
  fmap (testGroup "Message generation") $
  do testList <- flip evalStateT emptyMsgContext $
              do prepMsgGen
                 sequence [testActionMsgs
                          , testStringMsg
                          , addTwoIntsServiceMD5]
     return testList
