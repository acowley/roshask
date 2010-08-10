{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Msg.Parse (parseMsg) where
import Prelude hiding (takeWhile)
import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toLower, digitToInt)
import Data.List (foldl')
import System.Environment (getEnvironment)
import System.FilePath (dropExtension, takeFileName, splitDirectories, (</>))
import System.Process (readProcess)
import Msg.Types

eatLine = manyTill anyChar (eitherP endOfLine endOfInput) *> skipSpace
parseName = skipSpace *> identifier <* eatLine <* try comment

identifier = B.cons <$> letter_ascii <*> takeWhile validChar
    where validChar c = or (map ($ c) [isDigit, isAlpha_ascii, (== '_'), (== '/')])

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


userTypeParser :: Parser (ByteString, MsgType)
userTypeParser = choice [userSimple, userVarArray, userFixedArray]

userSimple :: Parser (ByteString, MsgType)
userSimple = (\t name -> (name, RUserType t)) <$>
             identifier <*> (space *> parseName)

userVarArray :: Parser (ByteString, MsgType)
userVarArray = (\t name -> (name, RVarArray (RUserType t))) <$>
               identifier <*> (string "[]" *> space *> parseName)

userFixedArray :: Parser (ByteString, MsgType)
userFixedArray = (\t n name -> (name, RFixedArray n (RUserType t))) <$>
                 identifier <*> 
                 (char '[' *> parseInt <* char ']') <*> 
                 (space *> parseName)

fieldParsers = builtIns ++ [comment *> userTypeParser]
    where builtIns = concatMap (\f -> map ((comment *>) . f) simpleFieldTypes)
                               [simpleParser, fixedArrayParser, varArrayParser]

mkParser :: String -> String -> Parser Msg
mkParser sname lname = Msg sname lname "" <$> many (choice fieldParsers)

testMsg = "# Foo bar\n\n#   \nHeader header  # a header\nuint32 aNum # a number \n  # It's not important\ngeometry_msgs/PoseStamped[] poses\n"

test = feed (parse (mkParser "" "") testMsg) ""

-- Ensure that field names do not coincide with Haskell reserved words.
sanitize :: Msg -> Msg
sanitize (Msg sname lname md5 fields) = Msg sname lname md5 $
                                        map sanitizeField fields
    where sanitizeField ("data", t)   = ("_data", t)
          sanitizeField ("type", t)   = ("_type", t)
          sanitizeField ("class", t)  = ("_class", t)
          sanitizeField ("module", t) = ("_module", t)
          sanitizeField x             = x

addHash :: String -> Msg -> Msg
addHash hash (Msg sname lname _ fields) = Msg sname lname hash fields

genName :: FilePath -> String
genName f = let parts = splitDirectories f
                [pkg,_,msgFile] = drop (length parts - 3) parts
            in pkg ++ "/" ++ dropExtension msgFile

-- Use roslib/scripts/gendeps to compute the MD5 ROS uses to uniquely
-- identify versions of msg files.
genRosMD5 :: FilePath -> IO String
genRosMD5 fname = 
    do env <- getEnvironment
       let ros_root = case lookup "ROS_ROOT" env of
                        Just s -> s
                        Nothing -> error "Environment variable ROS_ROOT not set"
           gendeps = ros_root</>"core"</>"roslib"</>"scripts"</>"gendeps"
       init <$> readProcess gendeps ["-m", fname] "" 

parseMsg :: FilePath -> IO (Either String Msg)
parseMsg fname = do msgFile <- B.readFile fname
                    let --hash = pack . show . md5 . BL.fromChunks $ [msgFile]
                        shortName = dropExtension . takeFileName $ fname
                        longName = genName fname
                        parser = mkParser shortName longName
                    hash <- genRosMD5 fname
                    case feed (parse parser msgFile) "" of
                      Done leftOver msg
                          | B.null leftOver -> return . Right . 
                                               addHash hash . sanitize $ 
                                               msg
                          | otherwise -> return $ Left $ "Couldn't parse " ++ 
                                                         unpack leftOver
                      Fail _ ctxt err -> return $ Left err
                      Partial _ -> return $ Left "Incomplete msg definition"
