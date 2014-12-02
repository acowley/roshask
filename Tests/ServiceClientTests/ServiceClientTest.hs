{-# LANGUAGE CPP #-}
module Main where
import qualified Ros.Test_srvs.AddTwoIntsRequest as Req
import qualified Ros.Test_srvs.AddTwoIntsResponse as Res
import Ros.Test_srvs.EmptyRequest
import Ros.Test_srvs.EmptyResponse
import Ros.Service (callService)
import Ros.Service.ServiceTypes
import Test.Tasty
import Test.Tasty.HUnit
import GHC.Int
import qualified Data.Int as Int
import Ros.Internal.Msg.SrvInfo
import Ros.Internal.RosBinary
import Control.Applicative ((<$>))
import Control.Exception
import Test.HUnit.Tools

-- To run:
-- 1. start ros: run "roscore"
-- 2. in another terminal start the add_two_ints server:
--      python roshask/Tests/ServiceClientTests/add_two_ints_server.py
-- 3. in a new terminal make sure $ROS_MASTER_URI is correct and run
--      cabal test servicetest --show-details=always
type Response a = IO (Either ServiceResponseExcept a)

main :: IO ()
main = defaultMain $ testGroup "Service Tests" [
  addIntsTest 4 7
  , notOkTest 100 100
  , requestResponseDontMatchTest
  , noProviderTest
  , connectionHeaderBadMD5Test
  , emptyServiceTest]
       
addIntsTest :: GHC.Int.Int64 -> GHC.Int.Int64 -> TestTree
addIntsTest x y = testCase ("add_two_ints, add " ++ show x ++ " + " ++ show y) $
  do res <- callService "/add_two_ints" Req.AddTwoIntsRequest{Req.a=x, Req.b=y} :: Response  Res.AddTwoIntsResponse
     Right (Res.AddTwoIntsResponse (x + y)) @=? res

-- add_two_ints_server returns None (triggering the NotOkError) if both a and b are 100
notOkTest :: GHC.Int.Int64 -> GHC.Int.Int64 -> TestTree
notOkTest x y = testCase ("NotOKError, add_two_ints, add " ++ show x ++ " + " ++ show y) $
  do res <- callService "/add_two_ints" Req.AddTwoIntsRequest{Req.a=x, Req.b=y} :: Response Res.AddTwoIntsResponse
     Left (NotOkExcept "service cannot process request: service handler returned None") @=? res

-- tests that an error is returned if the server is not registered with the master
noProviderTest :: TestTree
noProviderTest = testCase ("service not registered error") $
  do res <- callService "/not_add_two_ints" Req.AddTwoIntsRequest{Req.a=x, Req.b=y} :: Response Res.AddTwoIntsResponse
     Left (MasterExcept "lookupService failed, code: -1, statusMessage: no provider") @=? res
  where
    x = 10
    y = 10


requestResponseDontMatchTest :: TestTree
requestResponseDontMatchTest =
  testGroup "check request and response" [testMd5, testName]
  where
    testMd5 = testCase ("check md5") $ do
      assertRaises "Failed to detect mismatch"
        (ErrorCall "Request and response type do not match")
        --(callService "/add_two_ints" (Req.AddTwoIntsRequest 1 1) :: IO (Either ServiceResponseExcept BadMD5))
        (callService "/add_two_ints" (Req.AddTwoIntsRequest 1 1) :: Response BadMD5)
    testName =  testCase ("check name") $ do
      assertRaises "Failed to detect mismatch"
        (ErrorCall "Request and response type do not match")
        (callService "/add_two_ints" (Req.AddTwoIntsRequest 1 1) :: IO (Either ServiceResponseExcept BadName))

connectionHeaderBadMD5Test :: TestTree
connectionHeaderBadMD5Test = testCase "connection header wrong MD5 error" $ 
  do res <- callService "/add_two_ints" $ BadMD5 10 :: Response BadMD5
     Left (ConHeadExcept "Connection header from server has error, connection header is: [(\"error\",\"request from [roshask]: md5sums do not match: [6a2e34150c00229791cc89ff309fff22] vs. [6a2e34150c00229791cc89ff309fff21]\")]") @=? res

emptyServiceTest :: TestTree
emptyServiceTest =
  testCase "emptyService" $
  do res <- callService "/empty_srv" $ EmptyRequest :: Response EmptyResponse
     Right (EmptyResponse) @=? res

data BadMD5 = BadMD5 {a :: Int.Int64} deriving (Show, Eq)

instance SrvInfo BadMD5 where
  srvMD5 _ = "6a2e34150c00229791cc89ff309fff22"
  srvTypeName _ = "test_srvs/AddTwoInts"

instance RosBinary BadMD5 where
  put obj' = put (a obj')
  get = BadMD5 <$> get

data BadName = BadName Int.Int64 deriving (Show, Eq)

instance SrvInfo BadName where
  srvMD5 _ = "6a2e34150c00229791cc89ff309fff21"
  srvTypeName _ = "test_srvs/AddTwoIntu"

instance RosBinary BadName where
  put (BadName x) = put x
  get = BadName <$> get

#if MIN_VERSION_base(4,7,0)
#else
instance Eq ErrorCall where
    x == y = (show x) == (show y)
#endif
