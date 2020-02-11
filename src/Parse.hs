-- ~ module Parse
    -- ~ ( readExpr
    -- ~ , parseArray
    -- ~ , parseMatrix
    -- ~ ) where

module Parse
    ( module Parse
    , module Parse.Types
    ) where

import Control.Monad
import Data.Functor

import Parsing

import Parse.Primitives
import Parse.Operations
import Parse.Types

{-
Operator precedence

-       -- unary minus
()      -- paren/subexpression
^       -- exponent
*/      -- multiplication/division
+-      -- addition/subtraction

=       -- assignment
-}

readExpr :: String -> [(ParseTree,String)]
readExpr = parse parseLine

parseLine :: Parser ParseTree
parseLine = parseCmd
    <|> parseAssignment
    <|> parseDefun
    <|> (Expr' <$> parseExpr)
    <|> parseError

allTokens = token $ sat $ const True

parseError = Error <$> some allTokens

parseCmd = token $ char '@' *> (parseCmdQuit
                            <|> parseCmdHelp
                            <|> parseCmdPoly
                            <|> parseCmdReset)

parseCmdQuit = (string "quit" <|> string "exit") $> Command Quit

parseCmdHelp = string "help" $> Command Help

parseCmdPoly = string "poly" *> (Command . EvalPoly <$> many allTokens)

parseCmdReset = string "reset" $> Command Reset

-- ~ parseExpr :: Parser Expr
parseExpr = token $ parseOperation
    <|> parseFuncall
    <|> (Primitive' <$> parsePrimitive)
    <|> parseMatrix
    <|> parseArray
    <|> parseParenExpr

parseParenExpr = char '(' *> parseExpr <* char ')'

assignment = do
    name <- (Ident <$> identifier)
    char '='
    rhs <- parseExpr
    return (name, rhs)

parseAssignment = Assignment <$> token assignment

defun = do
    func <- parseFcall
    char '='
    rhs <- parseExpr
    return (func, rhs)

parseDefun = Defun <$> token defun

operand = parseFuncall
        <|> (Primitive' <$> parsePrimitive)
        <|> parseMatrix
        <|> parseArray
        <|> parseParenExpr      -- handle paren for cases like (1+2)^3

-- TODO: implement operator precidence
operation = do
        lhs <- operand
        op <- parseOperator
        rhs <- parseExpr
        return (op, lhs, rhs)
    <|> do          -- 4x = 4*x
        lhs <- operand
        rhs <- (Primitive' <$> parseIdentifier)
        return (Mult, lhs, rhs)

parseOperation = Operation <$> token operation

funcall =  let f = parseIdent <* char '('
    in do
        id <- f
        char ')'
        return (id, [])
    <|> do
        id <- f
        xs <- (:) <$> parseExpr <*> many (char ',' >> parseExpr)
        char ')'
        return (id, xs)

parseFcall = Fcall <$> token funcall

parseFuncall = Funcall <$> parseFcall

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
