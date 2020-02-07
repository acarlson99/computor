module Parser.Parser
    ( readExpr
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

parseExpr = parseNumber

readExpr :: String -> String
readExpr = show . parse parseFloat
