{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |Parses the ROS argument remapping syntax. This is the language used
-- to remap names and assign private parameter values from a command
-- line invocation of a Node.
module Ros.Internal.Util.ArgRemapping (parseRemappings, FromParam(..), 
                                       ParamVal) where
import Control.Applicative ((<$>))
import Control.Monad.Identity (Identity)
import Data.Either (partitionEithers, lefts, rights)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token
import Text.Parsec (letter, char, alphaNum, (<|>))
import Text.Parsec.Combinator (choice)
import Text.Parsec.Prim (Parsec, runParser)

-- |The types of values supported as parameters.
data ParamVal = PInt Int
              | PBool Bool
              | PString String
              | PDouble Double
              | PList [ParamVal]
              | PUnknown
                deriving Show

-- |Mechanism to extract a Haskell value from a parsed parameter
-- value.
class FromParam a where
    fromParam :: ParamVal -> a

instance FromParam Int where 
    fromParam (PInt x) = x
    fromParam x = error $ "Parameter is not an Int: " ++ show x

instance FromParam Bool where 
    fromParam (PBool x) = x
    fromParam x = error $ "Parameter is not a Bool: " ++ show x

instance FromParam String where 
    fromParam (PString x) = x
    fromParam x = error $ "Parameter is not a String: " ++ show x

instance FromParam Double where 
    fromParam (PDouble x) = x
    fromParam x = error $ "Parameter is not a Double: " ++ show x

-- NOTE: We have to provide specific instances for different Lists in
-- order to avoid overlapping with the String instance.
instance FromParam [Int] where 
    fromParam (PList xs) = map fromParam xs
    fromParam x = error $ "Parameter is not a List: " ++ show x

instance FromParam [Bool] where
    fromParam (PList xs) = map fromParam xs
    fromParam x = error $ "Parameter is not a List: " ++ show x

instance FromParam [Double] where
    fromParam (PList xs) = map fromParam xs
    fromParam x = error $ "Parameter is not a List: " ++ show x


-- | Name rebindings are typically used to remap Topic names, but can
-- also be used to remap a parameter name. 
type Names = [(String, String)]

-- | Parameter rebindings are used to assign values to private
-- parameters by prefixing the parameter name with an underscore. The
-- special keyword @__name@ can be used to remap the node name.
type Params = [(String, ParamVal)]

-- The ROS argument remapping syntax.
lexer :: GenTokenParser String u Identity
lexer = makeTokenParser $ 
        emptyDef { reservedNames = ["true", "false"] 
                 , identStart = letter <|> char '_' <|> char '/'
                 , identLetter = alphaNum <|> char '_' <|> char '/' }

data Sign = Positive | Negative

-- Parse an optional numeric sign character.
sign :: Parsec String () Sign
sign =     (char '-' >> return Negative) 
       <|> (char '+' >> return Positive) 
       <|> return Positive

applySign :: Num a => Sign -> a -> a
applySign Positive = id
applySign Negative = negate

-- Parses optionally signed integer or floating point numbers.
intOrFloat' :: Parsec String () (Either Int Double)
intOrFloat' = do s <- sign
                 num <- naturalOrFloat lexer
                 case num of
                   Left x -> return . Left $ applySign s (fromIntegral x)
                   Right x -> return . Right $ applySign s x

parseVal :: Parsec String () ParamVal
parseVal = choice [ either PInt PDouble <$> intOrFloat'
                  , const (PBool True) <$> reserved lexer "true"
                  , const (PBool False) <$> reserved lexer "false" 
                  , PString <$> (stringLiteral lexer <|> identifier lexer)
                  , PList <$> brackets lexer (commaSep lexer parseVal) ]

parseBinding :: Parsec String () (Either (String, String) (String, ParamVal))
parseBinding = do name <- identifier lexer 
                  _ <- symbol lexer ":="
                  case head name of
                    '_' -> do val <- parseVal
                              return $ Right (name, val)
                    _ -> do val <- identifier lexer
                            return $ Left (name, val)

-- |Parse program arguments to determine name remappings and parameter
-- settings.
parseRemappings :: [String] -> (Names, Params)
parseRemappings args = if null errors
                       then partitionEithers (rights remaps)
                       else error $ "Couldn't parse remapping "++ show errors
    where remaps = map (runParser parseBinding () "") args
          errors = lefts remaps

-- parseRemappings :: String -> (Names, Params)
-- parseRemappings args = case runParser (many1 parseBinding) () "" args of
--                          Left m -> error (show m)
--                          Right bindings -> partitionEithers bindings
{-
test :: (Names, Params)
test = parseRemappings [ "_joe:=42.1"
                       , "chatter:=/foo"
                       , "_susy:=[1,2,3]"
                       , "__name:=\"toby\"" 
                       , "_topic:=bar" ]
-}