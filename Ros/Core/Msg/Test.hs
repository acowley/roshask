import qualified Data.ByteString.Char8 as B
import Ros.Core.Msg.Parse
import Ros.Core.Msg.Gen

main = do parseResult <- parseMsg "LaserScan.msg"
          let txt = case parseResult of
                      Right msg -> generateMsgType msg
                      Left err -> error err
          putStr (B.unpack txt)
          B.writeFile "LaserScan.hs" txt