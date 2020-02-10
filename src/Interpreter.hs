module Interpreter
    ( interpret
    , emptyState
    ) where

import System.Console.Readline

import qualified Poly.Solve as P

import Parse.Parse
import Parse.Types

newtype CalcState = C [String]

emptyState :: CalcState
emptyState = C []

parseInput :: [(ParseTree,String)] -> (Int -> IO ())
parseInput [(Command Quit,"")] = \_ -> return ()
parseInput inp =
    (\ln -> do putStrLn $ show inp
               interpret ln emptyState)

interpret :: Int -> CalcState -> IO ()
interpret linenum _ = do
    maybeLine <- readline $ show linenum ++ " > "
    case maybeLine of
        Nothing -> return ()
        Just ln -> do addHistory ln
                      parseInput (readExpr ln) (linenum + 1)
