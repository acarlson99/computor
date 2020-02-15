module Eval.Math
    ( BaseType(..)
    , applyOp
    )
where

import           Data.Matrix

import qualified Types                         as T

import           Parse.Types

data BaseType = Int Int
              | Flt Float
              | Cpx (T.Complex Float)
              | Arr [BaseType]
              | Mtx (Matrix BaseType)
              deriving (Show,Eq)

showType (Int _) = "Int"
showType (Flt _) = "Float"
showType (Cpx _) = "Complex"
showType (Arr _) = "Array"
showType (Mtx _) = "Matrix"

invalidInstruction op lhs rhs =
    Left
        $  "Invalid types for instruction: "
        ++ showType lhs
        ++ " "
        ++ show op
        ++ " "
        ++ showType rhs

complexDiv (T.Complex (xa, xb)) (T.Complex (ya, yb)) = T.Complex (real, imag)
  where
    real = (xa * ya + xb * yb) / (ya * ya + yb * yb)
    imag = (xb * ya - xa * yb) / (ya * ya + yb * yb)

-- apply operation (Add,Mult,...) to BaseTypes erroring if unsupported
applyOp Add  (Int lhs) (Int rhs) = return $ Int $ lhs + rhs
applyOp Sub  (Int lhs) (Int rhs) = return $ Int $ lhs - rhs
applyOp Mult (Int lhs) (Int rhs) = return $ Int $ lhs * rhs
applyOp Div  (Int lhs) (Int rhs) = return $ Int $ lhs `div` rhs
applyOp Exp  (Int lhs) (Int rhs) = return $ Int $ lhs ^ rhs

applyOp Add  (Flt lhs) (Flt rhs) = return $ Flt $ lhs + rhs
applyOp Sub  (Flt lhs) (Flt rhs) = return $ Flt $ lhs - rhs
applyOp Mult (Flt lhs) (Flt rhs) = return $ Flt $ lhs * rhs
applyOp Div  (Flt lhs) (Flt rhs) = return $ Flt $ lhs / rhs
applyOp Exp  (Flt lhs) (Flt rhs) = return $ Flt $ lhs ** rhs
applyOp op   (Flt lhs) (Int rhs) = applyOp op (Flt lhs) (Flt (fromIntegral rhs))
applyOp op   (Int lhs) (Flt rhs) = applyOp op (Flt (fromIntegral lhs)) (Flt rhs)

applyOp Add  (Cpx lhs) (Cpx rhs) = return $ Cpx $ lhs + rhs
applyOp Sub  (Cpx lhs) (Cpx rhs) = return $ Cpx $ lhs - rhs
applyOp Mult (Cpx lhs) (Cpx rhs) = return $ Cpx $ lhs * rhs
applyOp Div  (Cpx lhs) (Cpx rhs) = return $ Cpx $ lhs `complexDiv` rhs
applyOp Exp  (Cpx lhs) rhs       = invalidInstruction Exp (Cpx lhs) rhs
applyOp Exp  lhs       (Cpx rhs) = invalidInstruction Exp lhs (Cpx rhs)
applyOp op (Int lhs) (Cpx rhs) =
    applyOp op (Cpx $ T.Complex (fromIntegral lhs, 0)) (Cpx rhs)
applyOp op (Cpx lhs) (Int rhs) =
    applyOp op (Cpx lhs) (Cpx $ T.Complex (fromIntegral rhs, 0))
applyOp op (Flt lhs) (Cpx rhs) =
    applyOp op (Cpx $ T.Complex (lhs, 0)) (Cpx rhs)
applyOp op (Cpx lhs) (Flt rhs) =
    applyOp op (Cpx lhs) (Cpx $ T.Complex (rhs, 0))
applyOp op lhs rhs = invalidInstruction op lhs rhs
