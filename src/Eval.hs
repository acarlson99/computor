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



-- apply operation (Add,Mult,...) to BaseTypes erroring if unsupported
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
    maybeToEither ("undefined variable: " ++ ident) $ M.lookup ident $ getVars
        st

evalExpr st (Array xs) = Arr <$> traverse (evalExpr st) xs
evalExpr st (Matrix xs) = Mtx <$> mapM (evalExpr st) xs

evalExpr st (Funcall (Fcall (Ident ident, xs))) = do
    -- get function args and body or error if undefined
    (args, body) <-
        maybeToEither ("undefined function: " ++ ident)
        $ M.lookup ident
        $ getFuncs st
    -- check arg list against supplied args
    if (length xs) /= (length args)
        then Left $ "Not enough arguments in function call: " ++ show
            (Funcall (Fcall (Ident ident, xs)))
        else do
            -- evaluate args, erroring on failure
            newArgs <- mapM (evalExpr st) xs
            -- evaluate body with state updated with args
            evalExpr
                ( foldr (\(Ident ident, val) st -> assignVar st ident val) st
                $ zip args newArgs
                )
                body
evalExpr st (Operation (op, lhs, rhs)) = do
    lhs' <- evalExpr st lhs
    rhs' <- evalExpr st rhs
    applyOp op lhs' rhs'

-- assignation, function definition, expression evaluation
-- updates state with function defs, vars
evalInput :: ParseTree -> CalcState -> Either String (CalcState, IO ())
evalInput (Expr' expr) st = do
    res <- evalExpr st expr
    return (st, print res)
evalInput (Defun (Ident fn, args, body)) st =
    Right (assignFun st fn (args, body), print $ Defun (Ident fn, args, body))
evalInput (Assignment (Ident ident, body)) st = case evalExpr st body of
    Right v   -> return (assignVar st ident v, print $ Assignment (Ident ident, body))
    Left  err -> Left err
evalInput (Error str) st = Left $ "unrecognized value " ++ str
evalInput expr        st = return (st, print expr)

-- match parsetree and evaluate, returning new state
eval :: [(ParseTree, String)] -> CalcState -> Either String (CalcState, IO ())
eval [] st = return (st, return ())
eval [(expr, x : xs)] st = Left $ "unparsed tokens: '"
        ++ (x : xs)
        ++ "' in expression '"
        ++ show expr
        ++ "'"
eval (x : y : ys) st = Left $ "WAIT WTF THIS SHOULD NOT HAPPEN" ++ show (x : y : ys)
eval [(expr, "")] st = evalInput expr st
