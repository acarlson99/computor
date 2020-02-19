module State
    ( State
    , emptyState
    , assignFun
    , getFun
    , assignVar
    , getVar
    )
where

import qualified Data.Map                      as M
import           Parse.Types
import           Math                           ( BaseType )

data State = State { getFuncs :: M.Map String ([Ident], Expr)
                           , getVars :: M.Map String BaseType }

instance Show State where
    show st =
        let fnc = M.toList $ getFuncs st
            var = M.toList $ getVars st
        in  foldr
                    (\(n, (args, expr)) acc ->
                        show (Defun (Ident n, args, expr)) ++ '\n' : acc
                    )
                    ""
                    fnc
                ++ foldr
                       (\(n, expr) acc -> n ++ " = " ++ show expr ++ '\n' : acc)
                       ""
                       var

emptyState :: State
emptyState = State M.empty M.empty

assignFun :: State -> String -> ([Ident], Expr) -> State
assignFun st ident val = State (M.insert ident val (getFuncs st)) (getVars st)

getFun :: State -> String -> Maybe ([Ident], Expr)
getFun st ident = M.lookup ident $ getFuncs st

assignVar :: State -> String -> BaseType -> State
assignVar st ident val = State (getFuncs st) (M.insert ident val (getVars st))

getVar :: State -> String -> Maybe BaseType
getVar st ident = M.lookup ident $ getVars st
