module Eval
    ( eval
    , evalArr
    )
where

import           Control.Monad

import           Util
import           Matrix
import           Math
import           State

import           Parse.Types

evalArray :: State -> Expr -> Either String [BaseType]
evalArray st (Array xs) = traverse (evalExpr st) xs
evalArray _  n          = Left $ "Invalid type to evalArray " ++ show n

constructMtx :: [[BaseType]] -> Either String (Matrix BaseType)
constructMtx [[]] = Right $ matrix 0 0 $ const (Int 0)
constructMtx xs   = do
    let lens = map length xs
    if maximum lens == minimum lens
        then Right $ fromLists xs
        else Left "Unable to construct matrix. Dimension mismatch"

evalExpr :: State -> Expr -> Either String BaseType
-- primitives evaluate to themselves
evalExpr _ (Primitive' n) = case n of
    (Number  n') -> return $ Int n'
    (Float   n') -> return $ Flt n'
    (Complex n') -> return $ Cpx n'
-- conditional evaluation
evalExpr st (Cond c good bad) = do
    cres <- evalExpr st c
    if cres == 0 then evalExpr st bad else evalExpr st good
-- identifiers query table of vars
evalExpr st (Identifier (Ident ident)) =
    maybeToEither ("undefined variable: " ++ ident) $ getVar st ident

-- arrays/matrixs construct matrices
evalExpr st (Array xs) = do
    mtx <- sequence [mapM (evalExpr st) xs]
    Mtx <$> constructMtx mtx
evalExpr st (Matrix xs) = do
    mtx <- mapM (evalArray st) xs
    Mtx <$> constructMtx mtx

evalExpr st (Funcall (Fcall (Ident ident, xs))) = do
    -- get function args and body or error if undefined
    (args, body) <- maybeToEither ("undefined function: " ++ ident)
        $ getFun st ident
    -- check arg list against supplied args
    if length xs /= length args
        then
            Left
            $  "Invalid number of arguments in function call: "
            ++ show (Funcall (Fcall (Ident ident, xs)))
            ++ " expected "
            ++ show (length args)
            ++ " got "
            ++ show (length xs)
        else do
            -- evaluate args, erroring on failure
            newArgs <- mapM (evalExpr st) xs
            -- evaluate body with state updated with args
            evalExpr
                (foldr (\(Ident ident', val) st' -> assignVar st' ident' val) st
                $ zip args newArgs
                )
                body
-- Operation (+, Int 7, Int 4) = Int 11
evalExpr st (Operation (op, lhs, rhs)) = do
    lhs' <- evalExpr st lhs
    rhs' <- evalExpr st rhs
    case applyOp op lhs' rhs' of
        Right v -> return v
        Left err ->
            Left
                $  err
                ++ " in expression `"
                ++ show lhs'
                ++ ' '
                :  show op
                ++ ' '
                :  show rhs'
                ++ "`"

-- assignation, function definition, expression evaluation
-- updates state with function defs, vars
evalInput :: ParseTree -> State -> Either String (State, IO ())
evalInput (Expr' expr) st = do
    res <- evalExpr st expr
    return (st, print res)
evalInput (Defun (Ident fn, args, body)) st = Right
    (assignFun st fn (args, body), print $ Defun (Ident fn, args, body))
evalInput (Assignment (Ident ident, body)) st = case evalExpr st body of
    Right v ->
        return (assignVar st ident v, putStrLn $ ident ++ " = " ++ show v)
    Left err -> Left err
evalInput (Error   str) _  = Left $ "unrecognized value " ++ str
evalInput (Command cmd) st = return (st, print (Command cmd))
evalInput EOL           st = return (st, putStrLn "")

-- match parsetree and evaluate, returning new state && IO
eval :: [(ParseTree, String)] -> State -> Either String (State, IO ())
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

evalArr :: [[(ParseTree, String)]] -> State -> Either String State
evalArr [] st = return st
evalArr xs st = foldM func st xs
  where
    func st' expr = do
        (newSt, _) <- eval expr st'
        return newSt
