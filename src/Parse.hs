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

parseComment :: Parser ParseTree
parseComment = token (char '#') *> many allTokens $> EOL

parseLine :: Parser ParseTree
parseLine = do
    line <-
        parseCmd
        <|> parseAssignment
        <|> parseDefun
        <|> (Expr' <$> parseExpr)
        <|> parseComment
        <|> parseError
    _ <- many parseComment
    return line

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
        <|> parseConditional
        <|> parseFuncall
        <|> (Primitive' <$> parsePrimitive)
        <|> parseMatrix
        <|> parseIdentifier
        <|> parseParenExpr
        <|> do
                _    <- token (char '-')
                expr <- parseExpr
                return $ Operation (Mult, expr, Primitive' $ Number (-1))

parseConditional :: Parser Expr
parseConditional = do
    _    <- token $ char '{'
    cond <- parseExpr
    _    <- token $ char '}'
    good <- parseExpr
    Cond cond good <$> parseExpr

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


---------------------------------- operators -----------------------------------

{-
^           -- exponent
** * / %    -- mtx-mult/multiplication/division/mod
+ -         -- addition/subtraction
< > <= >=   -- comparators
|| &&       -- and/or
-}

operation :: Parser Expr
operation = logOper

logOp :: Parser String
logOp = string "||" <|> string "&&"

parseLogOp :: Parser Operator
parseLogOp = strToOperator <$> token logOp

operator0 :: Parser String
operator0 =
    string ">=" <|> string "<=" <|> string "<" <|> string ">" <|> string "=="

parseOperator0 :: Parser Operator
parseOperator0 = strToOperator <$> token operator0

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

--------------------------- operations PEMDAS/BEDMAS ---------------------------

-- `rhsf` should ONLY consume the token used as rhs of the expression
-- in expression `1 - 2 - 3 - 4` functions should consume `1` `-` `2` only
parseOpLeftAssoc :: (Monad m, Alternative m) => m Expr -> m Operator -> m Expr -> m Expr
parseOpLeftAssoc lhsf opf rhsf = do
    lhs <- lhsf
    maybeAddSuffix lhs lhsf opf rhsf
  where
    addSuffix lhs' lhsf' opf' rhsf' = do
        op  <- opf'
        rhs <- rhsf'
        maybeAddSuffix (Operation (op, lhs', rhs)) lhsf' opf' rhsf'
    maybeAddSuffix e lhsf' opf' rhsf' = addSuffix e lhsf' opf' rhsf' <|> return e

-- && ||
logOper :: Parser Expr
logOper =
    parseOpLeftAssoc operation0 parseLogOp (operation0 <|> operand)
        <|> operation0
        <|> parseOpLeftAssoc operand parseLogOp (operation0 <|> operand)

-- comparators
operation0 :: Parser Expr
operation0 =
    parseOpLeftAssoc operation1 parseOperator0 (operation1 <|> operand)
        <|> operation1
        <|> parseOpLeftAssoc operand parseOperator0 (operation1 <|> operand)

-- + -
operation1 :: Parser Expr
operation1 =
    parseOpLeftAssoc operation2 parseOperator1 (operation2 <|> operand)
        <|> operation2
        <|> parseOpLeftAssoc operand parseOperator1 (operation2 <|> operand)

-- * / ** %
operation2 :: Parser Expr
operation2 =
    parseOpLeftAssoc operation3 parseOperator2 (operation3 <|> operand)
        <|> operation3
        <|> parseOpLeftAssoc operand parseOperator2 (operation3 <|> operand)

-- ^
-- to make right-associative
operation3 :: Parser Expr
operation3 = do
    lhs <- operand
    op  <- parseOperator3
    rhs <- (operation3 <|> operand)
    return $ Operation (op, lhs, rhs)

parseOperation :: Parser Expr
parseOperation = token operation

--------------------------------------------------------------------------------

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
