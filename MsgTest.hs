import MsgParse
import MsgGen

main = do parseResult <- parseMsg "LaserScan.msg"
          let txt = case parseResult of
                      Right msg -> generateMsgType msg
                      Left err -> error err
          putStr txt
          writeFile "LaserScan.hs" txt