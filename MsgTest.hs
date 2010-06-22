import MsgParse
import MsgGen

main = do parseResult <- parseMsg "LaserScan.msg"
          case parseResult of
            Right msg -> putStr (generateMsgType msg)
            Left err -> putStrLn err