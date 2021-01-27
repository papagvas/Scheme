module Lib
    ( 
      readExpr
    , eval
    , showVal
    ) where

import           Control.Monad.Except
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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNumnpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
