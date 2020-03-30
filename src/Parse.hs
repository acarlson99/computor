module Parse
    ( readExpr
    , parseIdentifier
    , parseArray
    , parseMatrix
    , parseOperation
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

----------------------------------- commands -----------------------------------

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

------------------------------------- expr -------------------------------------

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

----------------------------- assignment/function ------------------------------

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

--------------------------------- array/matrix ---------------------------------

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

---------------------------------- operators -----------------------------------

{-
^           -- exponent
** * / %    -- mtx-mult/multiplication/division/mod
+ -         -- addition/subtraction
< > <= >=   -- comparators
|| &&       -- and/or
-}

logOperator :: Parser String
logOperator = string "||" <|> string "&&"

parseLogOperator :: Parser Operator
parseLogOperator = strToOperator <$> token logOperator


compOperator :: Parser String
compOperator =
    string ">=" <|> string "<=" <|> string "<" <|> string ">" <|> string "=="

parseCompOperator :: Parser Operator
parseCompOperator = strToOperator <$> token compOperator


addSubOperator :: Parser String
addSubOperator = string "+" <|> string "-"

parseAddSubOperator :: Parser Operator
parseAddSubOperator = strToOperator <$> token addSubOperator


mulDivOperator :: Parser String
mulDivOperator = string "**" <|> string "*" <|> string "/" <|> string "%"

parseMulDivOperator :: Parser Operator
parseMulDivOperator = strToOperator <$> token mulDivOperator


expOperator :: Parser String
expOperator = string "^"

parseExpOperator :: Parser Operator
parseExpOperator = strToOperator <$> token expOperator

--------------------------- operations PEMDAS/BEDMAS ---------------------------

operand :: Parser Expr
operand =
    parseFuncall
        <|> (Primitive' <$> parsePrimitive)
        <|> parseIdentifier
        <|> parseMatrix
        <|> parseParenExpr      -- handle paren for cases like (1+2)^3

operation :: Parser Expr
operation = logOper

parseOperation :: Parser Expr
parseOperation = token operation


-- `rhsf` should ONLY consume the token used as rhs of the expression
-- in expression `1 - 2 - 3 - 4` functions should consume `1` `-` `2` only
parseOpLeftAssoc
    :: (Monad m, Alternative m) => m Expr -> m Operator -> m Expr -> m Expr
parseOpLeftAssoc lhsf opf rhsf = do
    lhs <- lhsf
    maybeAddSuffix lhs lhsf opf rhsf
  where
    addSuffix lhs' lhsf' opf' rhsf' = do
        op  <- opf'
        rhs <- rhsf'
        maybeAddSuffix (Operation (op, lhs', rhs)) lhsf' opf' rhsf'
    maybeAddSuffix e lhsf' opf' rhsf' =
        addSuffix e lhsf' opf' rhsf' <|> return e

-- recursively parses given operand, operator, and next level of precedence
createParser
    :: (Alternative f, Monad f) => f Expr -> f Operator -> f Expr -> f Expr
createParser operandF opF nextF =
    parseOpLeftAssoc nextF opF (nextF <|> operandF)
        <|> nextF
        <|> parseOpLeftAssoc operandF opF (nextF <|> operandF)

-- && ||
logOper :: Parser Expr
logOper = createParser operand parseLogOperator compOper

-- comparators
compOper :: Parser Expr
compOper = createParser operand parseCompOperator addSubOper

-- + -
addSubOper :: Parser Expr
addSubOper = createParser operand parseAddSubOperator mulDivOper

-- * / ** %
mulDivOper :: Parser Expr
mulDivOper = createParser operand parseMulDivOperator expOper

-- ^
-- to make right-associative
expOper :: Parser Expr
expOper = do
    lhs <- operand
    op  <- parseExpOperator
    rhs <- (expOper <|> operand)
    return $ Operation (op, lhs, rhs)
