module Interpreter
    ( interpret
    ) where

import System.Console.Readline

import qualified Poly.Solve as P

import Parse
import Eval

handleCmd Quit st ln     = return ()
handleCmd Help st ln     = do print "HELP MSG"
                              interpret st ln
handleCmd (EvalPoly xs) st ln = do P.printRes $ P.solve xs
                                   interpret st ln
handleCmd Reset _ ln = interpret emptyState ln

evalExpr [(Command cmd,"")] st ln = handleCmd cmd st ln
evalExpr exp state lnum          = let (newst,pm) = eval exp state
                                   in do
                                   pm
                                   interpret newst lnum

-- ~ interpret :: a -> Int -> IO ()
interpret state linenum = do
    maybeLine <- readline $ show linenum ++ " > "
    case maybeLine of
        Nothing -> return ()
        Just ln -> do addHistory ln
                      evalExpr (readExpr ln) state (linenum + 1)
