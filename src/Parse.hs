-- ~ module Parse
    -- ~ ( readExpr
    -- ~ , parseArray
    -- ~ , parseMatrix
    -- ~ ) where

module Parse
    ( module Parse
    , module Parse.Types
    )
where

import           Control.Monad
import           Data.Functor

import           Parsing

import           Parse.Primitives
import           Parse.Operations
import           Parse.Types

readExpr :: String -> [(ParseTree, String)]
readExpr = parse parseLine

parseLine :: Parser ParseTree
parseLine =
    parseCmd
        <|> parseAssignment
        <|> parseDefun
        <|> (Expr' <$> parseExpr)
        <|> parseError

allTokens = token $ sat $ const True

parseError = Error <$> some allTokens

parseCmd =
    token
        $  char '@'
        *> (   parseCmdQuit
           <|> parseCmdHelp
           <|> parseCmdPoly
           <|> parseCmdDump
           <|> parseCmdReset
           )

parseCmdQuit = (string "quit" <|> string "exit") $> Command Quit

parseCmdHelp = string "help" $> Command Help

parseCmdPoly = string "poly" *> (Command . EvalPoly <$> many allTokens)

parseCmdReset = string "reset" $> Command Reset

parseCmdDump = string "dump" $> Command Dump

parseExpr =
    token
        $   parseOperation
        <|> parseFuncall
        <|> (Primitive' <$> parsePrimitive)
        <|> parseMatrix
        -- ~ <|> parseArray
        <|> parseIdentifier
        <|> parseParenExpr

parseParenExpr = char '(' *> parseExpr <* char ')'

assignment = do
    name <- Ident <$> identifier
    char '='
    rhs <- parseExpr
    return (name, rhs)

parseAssignment = Assignment <$> token assignment

defun =
    do
            func <- parseIdent
            token $ char '('
            rhs <- parseRhs
            return (func, [], rhs)
        <|> do
                func <- parseIdent
                token $ char '('
                x   <- parseIdent
                xs  <- many (char ',' *> parseIdent)
                rhs <- parseRhs
                return (func, x : xs, rhs)
  where
    parseRhs = do
        token $ char ')'
        token $ char '='
        parseExpr

parseDefun = Defun <$> token defun

operand =
    parseFuncall
        <|> (Primitive' <$> parsePrimitive)
        <|> parseIdentifier
        <|> parseMatrix
        -- <|> parseArray
        <|> parseParenExpr      -- handle paren for cases like (1+2)^3


{-
TODO: add operator precedence

-       -- unary minus
()      -- paren/subexpression
^       -- exponent
*/      -- multiplication/division
+-      -- addition/subtraction

=       -- assignment
-}

-- TODO: implement operator precidence
-- ~ operation = do
    -- ~ lhs <- operand
    -- ~ op  <- parseOperator
    -- ~ rhs <- parseExpr
    -- ~ return (op, lhs, rhs)

        -- Commented out because it caused weird bugs
        -- ~ <|> do          -- 4x = 4*x
                -- ~ lhs <- operand
                -- ~ rhs <- parseIdentifier
                -- ~ return (Mult, lhs, rhs)

parseOp' lhsf opf rhsf = do
    lhs <- lhsf
    op  <- opf
    rhs <- rhsf
    return $ Operation (op, lhs, rhs)

operation = operation1

operator1 = string "+" <|> string "-"
parseOperator1 = strToOperator <$> token operator1
operator2 = string "**" <|> string "*" <|> string "/"
parseOperator2 = strToOperator <$> token operator2
operator3 = string "^"
parseOperator3 = strToOperator <$> token operator3

operation1 =
    parseOp' operation2 parseOperator1 (operation1 <|> operand)
        <|> operation2
        <|> parseOp' operand parseOperator1 (operation1 <|> operand)

operation2 =
    parseOp' operation3 parseOperator2 (operation2 <|> operand)
        <|> operation3
        <|> parseOp' operand parseOperator2 (operation2 <|> operand)

operation3 = parseOp' operand parseOperator3 (operation3 <|> operand)

-- ~ operation2 = do
    -- ~ lhs <- ((Operation <$> operation3) <|> operand)
    -- ~ op <- strToOperator <$> token (string "*" <|> string "/")
    -- ~ rhs <- parseExpr
    -- ~ return (op, lhs, rhs)
    -- ~ <|> operation3

-- ~ operation3 = do
    -- ~ lhs <- operand
    -- ~ op <- strToOperator <$> token (string "+" <|> string "-")
    -- ~ rhs <- parseExpr
    -- ~ return (op, lhs, rhs)

-- ~ parseOperation = Operation <$> token operation
parseOperation = token operation

funcall =
    let f = parseIdent <* char '('
    in  do
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

parseArrOnDelim delim fn =
    do
            token $ char '['
            x  <- fn
            xs <- many (token (char delim) >> fn)
            token $ char ']'
            return $ x : xs
        <|> do
                token $ char '['
                token $ char ']'
                return []

array = parseArrOnDelim ',' parseExpr

parseArray = Array <$> array

matrix = parseArrOnDelim ';' parseArray

parseMatrix = Matrix <$> matrix

parseIdent = Ident <$> identifier

parseIdentifier = Identifier <$> parseIdent
