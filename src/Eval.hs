module Eval
    ( eval
    , CalcState
    , emptyState
    -- ~ , constructMtx
    )
where

import qualified Data.Map                      as M

import           Util
import           Matrix

import           Parse.Types

import           Eval.Math

data CalcState = CalcState { getFuncs :: M.Map String ([Ident], Expr)
                           , getVars :: M.Map String BaseType }

instance Show CalcState where
    show st =
        let fnc = M.toList $ getFuncs st
            var = M.toList $ getVars st
        in  foldr
                    (\(n, (args, expr)) acc ->
                        show (Defun (Ident n, args, expr)) ++ '\n':acc
                    )
                    ""
                    fnc
                ++ foldr
                       (\(n, expr) acc -> n ++ " = " ++ show expr ++ '\n':acc)
                       ""
                       var

emptyState :: CalcState
emptyState = CalcState M.empty M.empty

assignFun :: CalcState -> String -> ([Ident], Expr) -> CalcState
assignFun st ident val =
    CalcState (M.insert ident val (getFuncs st)) (getVars st)

assignVar :: CalcState -> String -> BaseType -> CalcState
assignVar st ident val =
    CalcState (getFuncs st) (M.insert ident val (getVars st))


evalArray :: CalcState -> Expr -> Either String [BaseType]
evalArray st (Array xs) = traverse (evalExpr st) xs
evalArray _  n          = Left $ "Invalid type to evalArray " ++ show n

constructMtx :: [[BaseType]] -> Either String (Matrix BaseType)
constructMtx [[]] = Right $ matrix 0 0 $ const (Int 0)
constructMtx xs   = do
    let lens = map length xs
    if maximum lens == minimum lens
        then Right $ fromLists xs
        else Left "Unable to construct matrix. Dimension mismatch"

evalExpr :: CalcState -> Expr -> Either String BaseType
evalExpr _ (Primitive' n) = case n of
    (Number  n') -> return $ Int n'
    (Float   n') -> return $ Flt n'
    (Complex n') -> return $ Cpx n'
evalExpr st (Identifier (Ident ident)) =
    maybeToEither ("undefined variable: " ++ ident) $ M.lookup ident $ getVars
        st

evalExpr st (Array xs) = do
    mtx <- sequence [mapM (evalExpr st) xs]
    Mtx <$> constructMtx mtx
-- ~ evalExpr st (Matrix xs) = Mtx <$> mapM (evalExpr st) xs
evalExpr st (Matrix xs) = do
    mtx <- mapM (evalArray st) xs
    Mtx <$> constructMtx mtx

evalExpr st (Funcall (Fcall (Ident ident, xs))) = do
    -- get function args and body or error if undefined
    (args, body) <-
        maybeToEither ("undefined function: " ++ ident)
        $ M.lookup ident
        $ getFuncs st
    -- check arg list against supplied args
    if length xs /= length args
        then Left $ "Not enough arguments in function call: " ++ show
            (Funcall (Fcall (Ident ident, xs)))
        else do
            -- evaluate args, erroring on failure
            newArgs <- mapM (evalExpr st) xs
            -- evaluate body with state updated with args
            evalExpr
                (foldr (\(Ident ident', val) st' -> assignVar st' ident' val) st
                $ zip args newArgs
                )
                body
evalExpr st (Operation (op, lhs, rhs)) = do
    lhs' <- evalExpr st lhs
    rhs' <- evalExpr st rhs
    case applyOp op lhs' rhs' of
        Right v -> return v
        Left err -> Left $ err ++ " in expression `" ++ show lhs' ++ ' ' : show op ++ ' ' : show rhs' ++ "`"

-- ~ eVALEXPR = evalExpr

-- assignation, function definition, expression evaluation
-- updates state with function defs, vars
evalInput :: ParseTree -> CalcState -> Either String (CalcState, IO ())
evalInput (Expr' expr) st = do
    res <- evalExpr st expr
    return (st, print res)
evalInput (Defun (Ident fn, args, body)) st = Right
    (assignFun st fn (args, body), print $ Defun (Ident fn, args, body))
evalInput (Assignment (Ident ident, body)) st = case evalExpr st body of
    Right v ->
        return (assignVar st ident v, print $ Assignment (Ident ident, body))
    Left err -> Left err
evalInput (Error str) _  = Left $ "unrecognized value " ++ str
evalInput expr        st = return (st, print expr)

-- match parsetree and evaluate, returning new state
eval :: [(ParseTree, String)] -> CalcState -> Either String (CalcState, IO ())
eval [] st = return (st, return ())
eval [(expr, x : xs)] _ =
    Left
        $  "unparsed tokens: '"
        ++ (x : xs)
        ++ "' in expression '"
        ++ show expr
        ++ "'"
eval (x : y : ys) _ =
    Left $ "WAIT WTF THIS SHOULD NOT HAPPEN" ++ show (x : y : ys)
eval [(expr, "")] st = evalInput expr st
