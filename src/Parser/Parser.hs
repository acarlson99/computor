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

parseExpr = parseIdentifier
    <|> parseComplex
    <|> parseFloat
    <|> parseNumber
-- TODO: add more expressions (paren, operation, funcall)

parseArrOnDelim delim fn = do
    char '['
    x <- fn
    xs <- many (char delim >> fn)
    char ']'
    return $ x:xs

parseArray = Array <$> parseArrOnDelim ',' parseExpr

parseMatrix = Matrix <$> parseArrOnDelim ';' parseArray

-- ~ parseExpr = parseNumber

readExpr :: String -> String
readExpr = show . parse parseExpr
