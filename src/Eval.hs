module Eval
    ( eval
    , emptyState
    ) where

import qualified Data.Map as M

import Util
import qualified Types as T

import Parse.Types

data CalcState = CalcState { getFuncs :: M.Map String ([Ident], Expr)
                           , getVars :: M.Map String Expr }

instance Show CalcState where
    show st = let fnc = M.toList $ getFuncs st
                  var = M.toList $ getVars st
            in
            foldr (\(n,(args,exp)) acc -> show (Defun (Ident n, args, exp)) ++ acc) "" fnc ++ ";"
            ++ foldr (\(n,exp) acc -> show (Assignment (Ident n, exp)) ++ acc) ";" var

emptyState :: CalcState
emptyState = CalcState M.empty M.empty

-- ~ evalExpr :: (Show a, Num a) => Expr -> Either String a
evalExpr st (Primitive' n) = return $ Primitive' n
evalExpr st (Identifier (Ident idnt)) = maybeToEither ("variable undefined: " ++ idnt) $ M.lookup idnt $ getVars st
evalExpr st (Array xs) = Array <$> traverse (evalExpr st) xs
evalExpr st (Matrix xs) = Matrix <$> mapM (evalExpr st) xs
-- NOTE: cannot test currently
evalExpr st (Funcall (Fcall (Ident idnt,xs))) = do
    (args,body) <- maybeToEither ("function undefined: " ++ idnt) $ M.lookup idnt $ getFuncs st
    -- TODO: assign variables in scope
    evalExpr st body
evalExpr st (Operation (op,lhs,rhs)) = do
    lhs' <- evalExpr st lhs
    rhs' <- evalExpr st rhs
    -- TODO: properly assign variables
    Left $ show op ++ " " ++ show lhs' ++ " " ++ show rhs'
-- ~ evalExpr n _ = return n
-- ~ evalExpr st exp = Left $ "unimplemented: " ++ show exp

evalInput :: ParseTree -> CalcState -> (CalcState, IO())
evalInput (Expr' expr) st = (st, print $ evalExpr st expr)
evalInput (Defun (Ident fn,args,body)) st = (CalcState (M.insert fn (args,body) (getFuncs st)) (getVars st)
                                                , print $ Defun (Ident fn,args,body))
evalInput (Assignment (Ident idnt,body)) st      = (CalcState (getFuncs st) (M.insert idnt body (getVars st))
                                                       ,  print $ Assignment (Ident idnt,body))
evalInput (Error str) st = (st, putStrLn $ "ERROR: " ++ str)
evalInput expr             st = (st, print expr)

eval :: [(ParseTree,String)] -> CalcState -> (CalcState, IO ())
eval []               st = (st, return ())
eval [(expr,x:xs)]    st = (st, putStrLn $ "ERROR: unrecognized tokens: '" ++ (x:xs) ++ "' in expression '" ++ show expr ++ "'")
eval (x:y:ys)         st = (st, putStrLn $ "WAIT WTF THIS SHOULD NOT HAPPEN" ++ show (x:y:ys))
eval [(expr,"")]      st = evalInput expr st
