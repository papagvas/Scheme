module Lib
    ( 
      readExpr
    ) where

import           Text.Megaparsec (oneOf, Parsec(..), parse, noneOf)
import qualified Data.Void as Void (Void(..))
import           Text.Megaparsec.Char (space1, letterChar, char)
import           Control.Monad.Combinators (many, space1, digitChar, (<|>))
--import qualified Data.Text as Text (Text(..))

type Parser = Parsec Void.Void String

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse (space1 >> symbol) "" input of
  Left err -> "No match for: " ++ show err
  Right str -> "Found value"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many $ symbolChar <|> oneOf "\"\n\t\r\\"
  char '"'
  return $ String str 

parseAtom :: Parser LispVal
parseAtom = do
  first <- letterChar <|> symbol
  leftovers <- many (letterChar <|> symbol <|> digitChar) -- perhaps replace letter + digit by alphaNumChar
  let atom = [first] ++ leftovers
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             otherwise -> Atom atom
            
parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseOct <|> parseBin <|> parseHex >>= return 
-- do
--   digits <- some digitChar
--   return . Number . read
-- or
--   some digitChar >>= return . Number . read   

parseDecimal1 :: Parser LispVal
parseDecimal1 = some digitChar >>= return . Number . read

parseDecimal2 :: Parser LispVal
parseDecimal2 = 

parseBool :: Parser LispVal
parseBool = do
  char '#'
  torf <- oneOf "tf"
  return $ case torf of
             't' -> Bool True
             'f' -> Bool False


                                         
