module Parse
    ( readExpr
    , parseIdentifier
    , parseArray
    , parseMatrix
    , module Parse.Types
    )
where

import           Data.Functor

import           Parsing

import           Parse.Primitives
import           Parse.Operand
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

allTokens :: Parser Char
allTokens = token $ sat $ const True

parseError :: Parser ParseTree
parseError = Error <$> some allTokens

parseCmd :: Parser ParseTree
parseCmd =
    token
        $  char '@'
        *> (   parseCmdQuit
           <|> parseCmdHelp
           <|> parseCmdPoly
           <|> parseCmdDump
           <|> parseCmdReset
           )

parseCmdQuit :: Parser ParseTree
parseCmdQuit = (string "quit" <|> string "exit") $> Command Quit


parseCmdHelp :: Parser ParseTree
parseCmdHelp = string "help" $> Command Help

parseCmdPoly :: Parser ParseTree
parseCmdPoly = string "poly" *> (Command . EvalPoly <$> many allTokens)

parseCmdReset :: Parser ParseTree
parseCmdReset = string "reset" $> Command Reset

parseCmdDump :: Parser ParseTree
parseCmdDump = string "dump" $> Command Dump

parseExpr :: Parser Expr
parseExpr =
    token
        $   parseOperation
        <|> parseFuncall
        <|> (Primitive' <$> parsePrimitive)
        <|> parseMatrix
        <|> parseIdentifier
        <|> parseParenExpr

parseParenExpr :: Parser Expr
parseParenExpr = char '(' *> parseExpr <* char ')'

assignment :: Parser (Ident, Expr)
assignment = do
    name <- Ident <$> identifier
    _    <- char '='
    rhs  <- parseExpr
    return (name, rhs)

parseAssignment :: Parser ParseTree
parseAssignment = Assignment <$> token assignment

defun :: Parser (Ident, [Ident], Expr)
defun =
    do
            func <- parseIdent
            _    <- token $ char '('
            rhs  <- parseRhs
            return (func, [], rhs)
        <|> do
                func <- parseIdent
                _    <- token $ char '('
                x    <- parseIdent
                xs   <- many (char ',' *> parseIdent)
                rhs  <- parseRhs
                return (func, x : xs, rhs)
  where
    parseRhs = do
        _ <- token $ char ')'
        _ <- token $ char '='
        parseExpr

parseDefun :: Parser ParseTree
parseDefun = Defun <$> token defun

operand :: Parser Expr
operand =
    parseFuncall
        <|> (Primitive' <$> parsePrimitive)
        <|> parseIdentifier
        <|> parseMatrix
        <|> parseParenExpr      -- handle paren for cases like (1+2)^3


{-
TODO: add operator precedence

^           -- exponent
** * / %    -- mtx-mult/multiplication/division/mod
+ -         -- addition/subtraction

-}

parseOp' :: Monad m => m Expr -> m Operator -> m Expr -> m Expr
parseOp' lhsf opf rhsf = do
    lhs <- lhsf
    op  <- opf
    rhs <- rhsf
    return $ Operation (op, lhs, rhs)

operation :: Parser Expr
operation = operation1

operator1 :: Parser String
operator1 = string "+" <|> string "-"

parseOperator1 :: Parser Operator
parseOperator1 = strToOperator <$> token operator1

operator2 :: Parser String
operator2 = string "**" <|> string "*" <|> string "/" <|> string "%"

parseOperator2 :: Parser Operator
parseOperator2 = strToOperator <$> token operator2

operator3 :: Parser String
operator3 = string "^"

parseOperator3 :: Parser Operator
parseOperator3 = strToOperator <$> token operator3

operation1 :: Parser Expr
operation1 =
    parseOp' operation2 parseOperator1 (operation1 <|> operand)
        <|> operation2
        <|> parseOp' operand parseOperator1 (operation1 <|> operand)

operation2 :: Parser Expr
operation2 =
    parseOp' operation3 parseOperator2 (operation2 <|> operand)
        <|> operation3
        <|> parseOp' operand parseOperator2 (operation2 <|> operand)

operation3 :: Parser Expr
operation3 = parseOp' operand parseOperator3 (operation3 <|> operand)

parseOperation :: Parser Expr
parseOperation = token operation

funcall :: Parser (Ident, [Expr])
funcall =
    let f = parseIdent <* char '('
    in  do
                ident' <- f
                _      <- char ')'
                return (ident', [])
            <|> do
                    ident' <- f
                    xs     <- (:) <$> parseExpr <*> many (char ',' >> parseExpr)
                    _      <- char ')'
                    return (ident', xs)

parseFcall :: Parser Fcall
parseFcall = Fcall <$> token funcall

parseFuncall :: Parser Expr
parseFuncall = Funcall <$> parseFcall

parseArrOnDelim :: Char -> Parser a -> Parser [a]
parseArrOnDelim delim fn =
    do
            _  <- token $ char '['
            x  <- fn
            xs <- many (token (char delim) >> fn)
            _  <- token $ char ']'
            return $ x : xs
        <|> do
                _ <- token $ char '['
                _ <- token $ char ']'
                return []

array :: Parser [Expr]
array = parseArrOnDelim ',' parseExpr

parseArray :: Parser Expr
parseArray = Array <$> array

matrix :: Parser [Expr]
matrix = parseArrOnDelim ';' parseArray

parseMatrix :: Parser Expr
parseMatrix = Matrix <$> matrix

parseIdent :: Parser Ident
parseIdent = Ident <$> identifier

parseIdentifier :: Parser Expr
parseIdentifier = Identifier <$> parseIdent
