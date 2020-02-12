module Eval
    ( eval
    , emptyState
    ) where

import Parse.Types

newtype CalcState = C [String]

emptyState :: CalcState
emptyState = C []

evalExpr (Primitive' n) = Right n
evalExpr _ = Left "Undefined"

eval :: [(ParseTree,String)] -> CalcState -> (CalcState, IO ())
eval []               st = (st, return ())
eval [(expr,x:xs)]    st = (st, putStrLn $ "ERROR: unrecognized tokens: '" ++ (x:xs) ++ "' in expression '" ++ show expr ++ "'")
eval (x:y:ys)         st = (st, putStrLn $ "WAIT WTF THIS SHOULD NOT HAPPEN" ++ show (x:y:ys))
eval [(Expr' expr,_)] st = (st, print $ evalExpr expr)
eval expr             st = (st, print expr)
