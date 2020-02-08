-- ~ module Parser.Primitive
    -- ~ ( parseNumber
    -- ~ , parseFloat
    -- ~ , parseComplex
    -- ~ , parseIdentifier

    -- ~ , parsePrimitive
    -- ~ , ParseTree (..)
    -- ~ ) where

module Parser.Primitive
    ( module Parser.Primitive
    ) where

import qualified Types as T
import Parser.Parsing

data ParseTree = Number Int
               | Float Float
               | Identifier String
               | Complex (T.Complex Float)

               | Array [ParseTree]                  -- [Value]
               | Matrix [ParseTree]                 -- [Array]
               | Funcall (ParseTree, [ParseTree])   -- (String, [Values])
               -- ~ | Expr [ParseTree]
               -- ~ | Assignment ([String], ParseTree)
               -- ~ | Operator Char
               -- ~ | Operation (Char, [ParseTree])
               deriving (Show)

parseNumber = Number <$> integer

parseIdentifier = Identifier <$> identifier

parseFloat' :: Parser Float
parseFloat' = do
    lhs <- fromIntegral <$> natural
    char '.'
    rhs <- many digit
    if rhs == ""
    then return $ lhs
    else return $ lhs + read ("0." ++ rhs)

float = do
    char '-'
    num <- parseFloat'
    return $ negate num
    <|> parseFloat'

floating = token float

parseFloat = Float <$> floating

intAsFloat = do
    n <- int
    return $ fromIntegral n

comp =
    T.complex <$> ((float <|> intAsFloat) >>= (\a -> char 'i' >> return a))
    <|> do
        token $ string "-i"
        return $ T.Complex (0,-1)
    <|> do
        char 'i'
        return $ T.Complex (0,1)

complex = token comp

parseComplex = Complex <$> complex

parsePrimitive =
        parseComplex
    <|> parseIdentifier
    <|> parseFloat
    <|> parseNumber
    -- ~ <|> do
        -- ~ char '('
        -- ~ x <- parsePrimitive
        -- ~ char '('
        -- ~ return x
