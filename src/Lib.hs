module Lib
    ( 
      readExpr
    , eval
    , showVal
    ) where

import qualified Data.Char as Char (toLower)
import           Text.Megaparsec (oneOf, Parsec(..), parse, noneOf, try)
import qualified Data.Void as Void (Void(..))
import qualified Numeric as Num (readHex, readOct, readFloat)
import           Control.Monad (liftM)
import           Text.Megaparsec.Char (space1, letterChar, char, string, string', digitChar, alphaNumChar, hexDigitChar, binDigitChar, octDigitChar, spaceChar, symbolChar)
import           Control.Monad.Combinators (many, (<|>), some, sepBy, sepEndBy)
--import qualified Data.Text as Text (Text(..))

type Parser = Parsec Void.Void String

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> LispVal
readExpr input = case parse (space1 >> parseExpr) "" input of
  Left err -> String $ "No match for: " ++ show err
  Right val -> val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             | Float Double

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
  (return .  Atom) atom
            
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
parseDecimal2 = do 
  try $ string "#d"
  xs <- some digitChar
  (return . Number . read) xs

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  xs <- some hexDigitChar
  (return . Number . hex2dec) xs

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  xs <- some octDigitChar
  (return . Number . oct2dec) xs

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  xs <- some binDigitChar
  (return . Number . bin2dec) xs

hex2dec :: String -> Integer
hex2dec = fst . head  . Num.readHex

oct2dec :: String -> Integer
oct2dec = fst . head . Num.readOct

bin2dec :: String -> Integer
bin2dec (x:"") = read [x]
bin2dec (x:xs) = 2 ^ (length (x:xs) - 1) * (read [x]) + bin2dec xs

parseBool :: Parser LispVal
parseBool = do
  char '#'
  torf <- oneOf "tf"
  return $ case torf of
             't' -> Bool True
             'f' -> Bool False

parseExpr :: Parser LispVal
parseExpr = foldl (<|>) (try parseChar) (map try [parseAtom, parseString, parseFloat, parseNumber, parseList, parseQuoted, parseBool]) 
            <|> do char '('
                   x <- (try parseList) <|> parseDotList
                   char ')'
                   return x

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  x <- spaceChar <|> alphaNumChar <|> charName 
  return $ Char x 

charName :: Parser Char
charName = do
  xs <- string' "space" <|> string' "newline"
  if map Char.toLower xs == "space" then return ' ' else return '\n'

parseFloat :: Parser LispVal
parseFloat = do
  xs <- some digitChar
  char '.'
  ys <- some digitChar
  (return . Float . fst . head . Num.readFloat) $ xs ++ "." ++ ys

parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepBy` space1

parseDotList :: Parser LispVal
parseDotList = do
  init <- parseExpr `sepEndBy` space1
  tail <- char '.' >> space1 >> parseExpr
  return $ DottedList init tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x] 

showVal :: LispVal -> String
showVal (String str) = "\"" ++ str ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList init tail) = "(" ++ unwordsList init ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal 
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
