module Parser
    ( readExpr
    -- ~ , symbol
    -- ~ , parseString
    -- ~ , parseAtom
    -- ~ , parseNumber
    -- ~ , parseFloat
    -- ~ , parseExpr
    ) where

-- ~ import Text.Megaparsec
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

import qualified Types as T

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Float Float
             | Complex (T.Complex Float)
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany (space <|> newline)

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

-- NOTE: add negative number parsing
-- NOTE: maybe consolidate parseNumber and parseFloat
parseNumber' :: Parser String
parseNumber' = many1 digit

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ parseNumber'

parseFloat' :: Parser String
parseFloat' = do
    n <- many1 digit
    char '.'
    n' <- many1 digit
    return $ n ++ '.':n'

parseFloat :: Parser LispVal
parseFloat = liftM (Float . read) parseFloat'

parseComplex :: Parser LispVal
parseComplex = do
    n <- (try parseFloat' <|> parseNumber')
    char 'i'
    return $ Complex $ T.Complex (0, read n)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = spaces >> (parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val
