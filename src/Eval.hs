module Eval
    ( eval
    , emptyState
    ) where

import Parse.Types

newtype CalcState = C [String]

emptyState :: CalcState
emptyState = C []

eval :: [(ParseTree,String)] -> CalcState -> (CalcState, IO ())
eval expr st = (st, print expr)
