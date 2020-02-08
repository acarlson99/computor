import Control.Monad

import Parsing

import Parser.Parser
import Parser.Primitives
import Parser.Types

import qualified Types as T

compInOut f _ (inp,outp) = let outp' = show $ parse f inp
    in if outp == outp'
    then Right True
    else fail $ "INP " ++ inp ++ " EXPECTED " ++ outp ++ " GOT " ++ outp'

test inps outps genf testf =
    foldM (compInOut testf) True
        $ zip inps $ map genf $ zip inps outps

runTest f = do
    case f of Left msg -> fail msg
              Right _  -> return ()

parseFloatTest =
    let inps  = [     "12.",     "12.3",      "-1." ,      "-1.4" ,     "15.6"
                ,     "0." ,     "0.5" ,      "-0.5",      "--1.0",     "10..10"
                ,     "-1.-1" ]
        outps = [ Just 12.0, Just 12.3 , Just (-1.0), Just (-1.4) , Just 15.6
                , Just 0.0 , Just 0.5  , Just (-0.5), Nothing     , Nothing
                , Just (-1)]
        gentest ("-1.-1",_) = show $ [(Float (-1.0), "-1")]
        gentest ("10..10",_) = show $ [(Float 10.0, ".10")]
        gentest (_,Just n) = show $ [(Float n, "")]
        gentest (_,Nothing) = "[]"
    in test inps outps gentest parseFloat

parseNumberTest =
    let inps  = [    "1",     "2",     "0",      "-1",   "--1",      "-100" ]
        outps = [Just 1 , Just 2 , Just 0 , Just (-1), Nothing, Just (-100) ]
        gentest (_, Just n)  = show $ [(Number n, "")]
        gentest (_, Nothing) = "[]"
    in test inps outps gentest parseNumber

parseIdentifierTest =
    let inps  = [      "abc",      "a12",  "+123" ]
        outps = [ Just "abc", Just "a12", Nothing ]
        gentest (i,Nothing) = "[]"
        gentest (i,_)       = show $ [(Identifier i, "")]
    in test inps outps gentest parseIdentifier

parseComplexTest =
    let inps  = [     "12i",     "3.2i",      "-2.4i",   "1.3" ]
        outps = [ Just 12  , Just 3.2  , Just (-2.4) , Nothing ]
        gentest (_,Nothing) = "[]"
        gentest (_,Just n)  = show [(Complex $ T.Complex (0,n), "")]
    in test inps outps gentest parseComplex

main :: IO ()
main = mapM_ runTest
    [ parseFloatTest
    , parseNumberTest
    , parseIdentifierTest
    , parseComplexTest
    ]
