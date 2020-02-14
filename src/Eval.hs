module Eval
    ( eval
    , emptyState
    )
where

import qualified Data.Map                      as M

import           Util
import qualified Types                         as T

import           Parse.Types

data BaseType = Int Int
              | Flt Float
              | Cpx (T.Complex Float)
              | Arr [BaseType]
              | Mtx [BaseType]
              deriving (Show,Eq)

data CalcState = CalcState { getFuncs :: M.Map String ([Ident], Expr)
                           , getVars :: M.Map String BaseType }

instance Show CalcState where
    show st =
        let fnc = M.toList $ getFuncs st
            var = M.toList $ getVars st
        in  foldr
                    (\(n, (args, exp)) acc ->
                        show (Defun (Ident n, args, exp)) ++ acc
                    )
                    ";"
                    fnc
                ++ foldr
                       (\(n, exp) acc -> show n ++ " = " ++ show exp ++ acc)
                       ";"
                       var

emptyState :: CalcState
emptyState = CalcState M.empty M.empty

assignFun st ident val =
    CalcState (M.insert ident val (getFuncs st)) (getVars st)

assignVar st ident val =
    CalcState (getFuncs st) (M.insert ident val (getVars st))




applyOp Add (Int lhs) (Int rhs) = return $ Int $ lhs + rhs
applyOp op lhs rhs =
    Left
        $  "Unimplemented instruction: "
        ++ show op
        ++ " "
        ++ show lhs
        ++ " "
        ++ show rhs

evalExpr :: CalcState -> Expr -> Either String BaseType
evalExpr _ (Primitive' n) = case n of
    (Number  n) -> return $ Int n
    (Float   n) -> return $ Flt n
    (Complex n) -> return $ Cpx n
evalExpr st (Identifier (Ident ident)) =
    maybeToEither ("variable undefined: " ++ ident) $ M.lookup ident $ getVars
        st

evalExpr st (Array xs) = Arr <$> traverse (evalExpr st) xs
evalExpr st (Matrix xs) = Mtx <$> mapM (evalExpr st) xs

evalExpr st (Funcall (Fcall (Ident ident, xs))) = do
    (args, body) <-
        maybeToEither ("function undefined: " ++ ident)
        $ M.lookup ident
        $ getFuncs st
    -- TODO: check arg list against supplied args
    newArgs <- mapM (evalExpr st) xs
    evalExpr
        ( foldr (\(Ident ident, val) st -> assignVar st ident val) st
        $ zip args newArgs
        )
        body
    -- ~ evalExpr st body
evalExpr st (Operation (op, lhs, rhs)) = do
    lhs' <- evalExpr st lhs
    rhs' <- evalExpr st rhs
    applyOp op lhs' rhs'



evalInput :: ParseTree -> CalcState -> (CalcState, IO ())
evalInput (Expr' expr) st = (st, print $ evalExpr st expr)
evalInput (Defun (Ident fn, args, body)) st =
    (assignFun st fn (args, body), print $ Defun (Ident fn, args, body))
evalInput (Assignment (Ident ident, body)) st = case evalExpr st body of
    Right v   -> (assignVar st ident v, print $ Assignment (Ident ident, body))
    Left  err -> (st, print err)
evalInput (Error str) st = (st, putStrLn $ "ERROR: unrecognized value " ++ str)
evalInput expr        st = (st, print expr)



eval :: [(ParseTree, String)] -> CalcState -> (CalcState, IO ())
eval [] st = (st, return ())
eval [(expr, x : xs)] st =
    ( st
    , putStrLn
        $  "ERROR: unparsed tokens: '"
        ++ (x : xs)
        ++ "' in expression '"
        ++ show expr
        ++ "'"
    )
eval (x : y : ys) st =
    (st, putStrLn $ "WAIT WTF THIS SHOULD NOT HAPPEN" ++ show (x : y : ys))
eval [(expr, "")] st = evalInput expr st
