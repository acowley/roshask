{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Msg.Parse (parseMsg) where
import Prelude hiding (takeWhile)
import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower, digitToInt)
import Data.List (foldl')
import System.FilePath (dropExtension, takeFileName)
import Msg.Types

eatLine = manyTill anyChar (eitherP endOfLine endOfInput) *> skipSpace
parseName = skipSpace *> identifier <* eatLine <* try comment

identifier = B.cons <$> letter_ascii <*> takeWhile validChar
    where validChar c = or (map ($ c) [isDigit, isAlpha_ascii, (== '_')])

parseInt = foldl' (\s x -> s*10 + digitToInt x) 0 <$> many1 digit

comment = many $ skipSpace *> try (char '#' *> eatLine)

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

fieldParsers = builtIns ++ [comment *> userTypeParser]
    where builtIns = concatMap (\f -> map ((comment *>) . f) simpleFieldTypes)
                               [simpleParser, fixedArrayParser, varArrayParser]

mkParser :: String -> Parser Msg
mkParser name = Msg name "" <$> many (choice fieldParsers)

testMsg = "# Foo bar\n\n#   \nHeader header  # a header\nuint32 aNum # a number \n  # It's not important"

test = feed (parse (mkParser "") testMsg) ""

-- Ensure that field names do not coincide with Haskell reserved words.
sanitize :: Msg -> Msg
sanitize (Msg name md5 fields) = Msg name md5 $
                                 map sanitizeField fields
    where sanitizeField ("data", t)   = ("_data", t)
          sanitizeField ("type", t)   = ("_type", t)
          sanitizeField ("class", t)  = ("_class", t)
          sanitizeField ("module", t) = ("_module", t)

parseMsg :: FilePath -> IO (Either String Msg)
parseMsg fname = do msgFile <- B.readFile fname
                    let parser = mkParser (dropExtension . takeFileName $ fname)
                    case feed (parse parser msgFile) "" of
                      Done leftOver msg
                          | B.null leftOver -> return . Right . sanitize $ msg
                          | otherwise -> return $ Left $ "Couldn't parse " ++ 
                                                         unpack leftOver
                      Fail _ ctxt err -> return $ Left err
                      Partial _ -> return $ Left "Incomplete msg definition"
