{-# LANGUAGE OverloadedStrings, TupleSections #-}
module MsgParse where
import Control.Applicative
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Char (toLower, digitToInt)
import Data.List (foldl')

data MsgType = RBool | RInt8 | RUInt8 | RInt16 | RUInt16
             | RInt32 | RUInt32 | RInt64 | RUInt64
             | RFloat32 | RFloat64 | RString | RTime | RDuration
             | RFixedArray Int MsgType | RVarArray MsgType
               deriving Show

data Msg = Msg String [(ByteString, MsgType)] deriving Show

parseName = takeTill (\c -> c == '\r' || c == '\n' || c == '\NUL')
parseInt = foldl' (\s x -> s*10 + digitToInt x) 0 <$> many1 digit

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

fieldParsers = concatMap (flip map simpleFieldTypes) $
               [simpleParser, fixedArrayParser, varArrayParser]

test = feed (parse parseMsg "bool bar\nint32 x\nfloat32[] y\nfloat64[3] z") ""

parseMsg :: Parser Msg
parseMsg = Msg "Anon" <$> choice fieldParsers `sepBy` char '\n'
