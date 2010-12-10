-- |Build all messages defined in a ROS package.
module Ros.Core.Msg.PkgBuilder where
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Either (rights)
import Data.List (findIndex)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import Ros.Core.Build.DepFinder (findMessages)
import Ros.Core.Msg.Analysis
import Ros.Core.Msg.Gen (generateMsgType)
import Ros.Core.Msg.Parse (parseMsg)

-- Build all messages defined by a package.
buildPkgMsgs :: FilePath -> MsgInfo ()
buildPkgMsgs fname = do liftIO . putStrLn $ "Generating package " ++ fname
                        liftIO $ createDirectoryIfMissing True destDir
                        pkgMsgs <- liftIO $ findMessages fname
                        let pkgMsgs' = map (B.pack . cap . dropExtension . takeFileName) 
                                           pkgMsgs
                            checkErrors xs = case findIndex isLeft xs of
                                               Nothing -> rights xs
                                               Just i -> err (pkgMsgs !! i)
                            names = map ((destDir </>) .
                                        flip replaceExtension ".hs" . 
                                        takeFileName)
                                        pkgMsgs
                            gen = generateMsgType pkgHier pkgMsgs'
                        parsed <- liftIO $ checkErrors <$> mapM parseMsg pkgMsgs
                        mapM_ (\(n, m) -> gen m >>= 
                                          liftIO . B.writeFile n)
                              (zip names parsed)
    where err pkg = error $ "Couldn't parse message " ++ pkg
          destDir = fname </> "msg" </> "haskell" </> "Ros" </> pkgName
          cap s = toUpper (head s) : tail s
          pkgName = cap . last . splitPath $ fname
          pkgHier = B.pack $ "Ros." ++ pkgName ++ "."
          isLeft (Left _) = True
          isLeft _ = False
