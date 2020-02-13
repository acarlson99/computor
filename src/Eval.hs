module Eval
    ( eval
    , emptyState
    ) where

import Data.Map
import qualified Data.Map as M

import Util
import qualified Types as T

import Parse.Types

data CalcState = CalcState { getFuncs :: M.Map String (Ident, [Ident], Expr)
                           , getIdent :: M.Map String Expr }

emptyState :: CalcState
emptyState = CalcState M.empty M.empty

-- ~ evalPrim :: Num a => Primitive -> CalcState -> Either String a
-- ~ evalPrim (Number n)             st = Right n
-- ~ evalPrim (Float f)              st = Right f
-- ~ evalPrim (Identifier (Ident s)) st = do
    -- ~ exp <- maybeToEither ("Unable to find identifier: " ++ s) $ M.lookup s $ getIdent st
    -- ~ evalExpr exp
evalPrim (Identifier (Ident s)) st = do
    exp <- maybeToEither ("Unable to find identifier: " ++ s) $ M.lookup s $ getIdent st
    evalExpr exp
-- ~ evalPrim (Complex c)            st = Right c
-- ~ evalPrim (Complex (T.Complex (r,c)))            st = return $ T.Complex (r,c)

-- ~ evalExpr :: (Show a, Num a) => Expr -> Either String a
evalExpr (Primitive' n) = return n
evalExpr exp = Left $ "unimplemented: " ++ show exp

eval :: [(ParseTree,String)] -> CalcState -> (CalcState, IO ())
eval []               st = (st, return ())
eval [(expr,x:xs)]    st = (st, putStrLn $ "ERROR: unrecognized tokens: '" ++ (x:xs) ++ "' in expression '" ++ show expr ++ "'")
eval (x:y:ys)         st = (st, putStrLn $ "WAIT WTF THIS SHOULD NOT HAPPEN" ++ show (x:y:ys))
eval [(Expr' expr,_)] st = (st, print $ evalExpr expr)
eval expr             st = (st, print expr)
