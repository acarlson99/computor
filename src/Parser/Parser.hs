module Parser.Parser
    ( readExpr
    , parseArray
    , parseMatrix
    ) where

import Control.Monad

import qualified Types as T
import Parser.Parsing
import Parser.Primitive

{-
Operator precedence

-       -- unary minus
()      -- paren/subexpression
^       -- exponent
*/      -- multiplication/division
+-      -- addition/subtraction

=       -- assignment
-}

-- ~ parseExpr :: Parser [ParseTree]
-- ~ parseExpr = do
    -- ~ x <- parsePrimitive
    -- ~ xs <- parseExpr
    -- ~ return $ x:xs
    -- ~ <|> do
    -- ~ xs <- parsePrimitive
    -- ~ return xs

parseExpr = parseFuncall
    <|> parsePrimitive
    <|> parseArray
    <|> parseMatrix
-- TODO: add more expressions (paren, operation, funcall)

funcall = do
    id <- parseIdentifier
    char '('
    char ')'
    return (id, [])

parseFuncall = Funcall <$> funcall

parseArrOnDelim delim fn = do
    char '['
    x <- fn
    xs <- many (char delim >> fn)
    char ']'
    return $ x:xs

array = parseArrOnDelim ',' parseExpr

parseArray = Array <$> array
-- ~ parseArray = Array <$> parseArrOnDelim ',' parseExpr

matrix = parseArrOnDelim ';' parseArray

parseMatrix = Matrix <$> matrix

-- ~ parseExpr = parseNumber

readExpr :: String -> String
readExpr = show . parse parseExpr
