-- ~ module Parser.Parser
    -- ~ ( readExpr
    -- ~ , parseArray
    -- ~ , parseMatrix
    -- ~ ) where

module Parser.Parser
    ( module Parser.Parser
    ) where

import Control.Monad
import Data.Functor

import Parsing

import Parser.Primitives
import Parser.Operations
import Parser.Types

{-
Operator precedence

-       -- unary minus
()      -- paren/subexpression
^       -- exponent
*/      -- multiplication/division
+-      -- addition/subtraction

=       -- assignment
-}

readExpr :: String -> String
readExpr = show . parse parseLine

parseLine = parseCmd
    <|> parseAssignment
    <|> parseDefun
    <|> parseExpr

parseCmd = token $ char '@' *> (parseCmdQuit <|> parseCmdHelp <|> parseCmdPoly)

parseCmdQuit = string "quit" $> Command Quit

parseCmdHelp = string "help" $> Command Help

poly :: Parser String
poly = many $ sat $ const True

parseCmdPoly = string "poly" *> (Command . EvalPoly <$> poly)

parseExpr = token $ parseOperation
    <|> parseFuncall
    <|> parsePrimitive
    <|> parseMatrix
    <|> parseArray
    <|> parseParenExpr

parseParenExpr = char '(' *> parseExpr <* char ')'

assignment = do
    name <- parseIdentifier
    char '='
    rhs <- parseExpr
    return (name, rhs)

parseAssignment = Assignment <$> token assignment

defun = do
    func <- parseFuncall
    char '='
    rhs <- parseExpr
    return (func, rhs)

parseDefun = Defun <$> token defun

operand = parseFuncall
        <|> parsePrimitive
        <|> parseMatrix
        <|> parseArray
        <|> parseParenExpr      -- handle paren for cases like (1+2)^3

operation = do
        lhs <- operand
        op <- parseOperator
        rhs <- parseExpr
        return (op, lhs, rhs)
    <|> do          -- 4x = 4*x
        lhs <- operand
        rhs <- parseIdentifier
        return (Mult, lhs, rhs)

parseOperation = Operation <$> token operation

funcall =  let f = parseIdentifier <* char '('
    in do
        id <- f
        char ')'
        return (id, [])
    <|> do
        id <- f
        xs <- (:) <$> parseExpr <*> many (char ',' >> parseExpr)
        char ')'
        return (id, xs)

parseFuncall = Funcall <$> token funcall

parseArrOnDelim delim fn = do
        char '['
        x <- fn
        xs <- many (char delim >> fn)
        char ']'
        return $ x:xs
    <|> do
        char '['
        char ']'
        return []

array = parseArrOnDelim ',' parseExpr

parseArray = Array <$> array
-- ~ parseArray = Array <$> parseArrOnDelim ',' parseExpr

matrix = parseArrOnDelim ';' parseArray

parseMatrix = Matrix <$> matrix
