{-# LANGUAGE OverloadedStrings, TupleSections #-}
module MsgParse (parseMsg) where
import Prelude hiding (takeWhile)
import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
--import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower, digitToInt)
import Data.List (foldl')
import System.FilePath (dropExtension, takeFileName)
import MsgTypes

eatLine = manyTill anyChar (eitherP endOfLine endOfInput) *> skipSpace
parseName = skipSpace *> identifier <* eatLine <* try eatNothings

identifier = B.cons <$> letter_ascii <*> takeWhile validChar
    where validChar c = or (map ($ c) [isDigit, isAlpha_ascii, (== '_')])

parseInt = foldl' (\s x -> s*10 + digitToInt x) 0 <$> many1 digit

comment = skipSpace *> try (char '#' *> eatLine)

eatNothings = many comment

--testFoo = feed (parse eatNothings "  #hi\n\n#guys\n\nfoo") ""
testFoo = feed (parse eatNothings " #hi  #hardy    #boo") ""

typeString :: MsgType -> Parser ByteString
typeString = string . pack . map toLower . tail . show

simpleFieldTypes = [ RBool, RInt8, RUInt8, RInt16, RUInt16, RInt32, RUInt32, 
                     RInt64, RUInt64, RFloat32, RFloat64, RString, 
                     RTime, RDuration ]

simpleParser :: MsgType -> Parser (ByteString, MsgType)
simpleParser x = (, x) <$> (typeString x *> space *> parseName)

fixedArrayParser x = (\len name -> (name, RFixedArray len x)) <$>
                     (typeString x *> char '[' *> parseInt <* char ']') <*> 
                     (space *> parseName)

varArrayParser x = (, RVarArray x) <$> 
                   (typeString x *> string "[]" *> space *> parseName)

userTypeParser = (\t name -> (name, RUserType t )) <$> 
                 takeTill (== ' ') <*> (space *> parseName)

-- fieldParsers = concatMap (flip map simpleFieldTypes) $
--                [simpleParser, fixedArrayParser, varArrayParser]
fieldParsers = builtIns ++ [eatNothings *> userTypeParser]
    where builtIns = concatMap (\f -> map ((eatNothings *>) . f) simpleFieldTypes)
                               [simpleParser, fixedArrayParser, varArrayParser]

--test = feed (parse parseMsg "bool bar\nint32 x\nfloat32[] y\nfloat64[3] z") ""
-- parseMsg :: Parser Msg
-- parseMsg = Msg "Anon" <$> choice fieldParsers `sepBy` char '\n'

testMsg = pack $ 
          "# LaserScan message blah\n" ++
          "#\n"++
          "Header header # timestamp in the header\n"++
          "              # froufrah\n"++
          "\n"++
          "float32 angle_min"
test = feed (parse (mkParser "Laser") testMsg) ""

mkParser :: String -> Parser Msg
mkParser name = Msg name <$> many (choice fieldParsers) -- `sepBy` char '\n'

parseMsg :: FilePath -> IO (Either String Msg)
parseMsg fname = do msgFile <- B.readFile fname
                    let parser = mkParser (dropExtension . takeFileName $ fname)
                    case feed (parse parser msgFile) "" of
                      Done leftOver msg
                          | B.null leftOver -> return $ Right msg
                          | otherwise -> return $ Left $ "Couldn't parse " ++ 
                                                         unpack leftOver
                      Fail _ ctxt err -> return $ Left err
                      Partial _ -> return $ Left "Incomplete msg definition"
