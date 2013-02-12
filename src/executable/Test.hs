import qualified Data.ByteString.Char8 as B
import Parse
import Gen

main = do parseResult <- parseMsg "LaserScan.msg"
          let txt = case parseResult of
                      Right msg -> generateMsgType msg
                      Left err -> error err
          putStr (B.unpack txt)
          B.writeFile "LaserScan.hs" txt