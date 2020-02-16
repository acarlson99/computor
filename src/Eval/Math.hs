module Eval.Math
    ( BaseType(..)
    , applyOp
    )
where

import           Data.Matrix
import           Control.Applicative

import qualified Types                         as T

import           Parse.Types

data BaseType = Int Int
              | Flt Float
              | Cpx (T.Complex Float)
              | Mtx (Matrix BaseType)
              deriving (Eq)

instance Show BaseType where
    show (Int n) = show n
    show (Flt n) = show n
    show (Cpx n) = show n
    show (Mtx n) = show n

instance Num BaseType where
    -- add
    (Int lhs) + (Int rhs) = Int $ lhs + rhs
    (Flt lhs) + (Flt rhs) = Flt $ lhs + rhs
    (Cpx lhs) + (Cpx rhs) = Cpx $ lhs + rhs
    (Mtx lhs) + (Mtx rhs) = Mtx $ lhs + rhs

    (Flt lhs) + (Int rhs) = Flt lhs + Flt (fromIntegral rhs)
    (Int lhs) + (Flt rhs) = Flt (fromIntegral lhs) + Flt rhs

    (Int lhs) + (Cpx rhs) = (Cpx $ T.Complex (fromIntegral lhs, 0)) + Cpx rhs
    (Cpx lhs) + (Int rhs) = Cpx lhs + (Cpx $ T.Complex (fromIntegral rhs, 0))
    (Flt lhs) + (Cpx rhs) = (Cpx $ T.Complex (lhs, 0)) + Cpx rhs
    (Cpx lhs) + (Flt rhs) = Cpx lhs + (Cpx $ T.Complex (rhs, 0))

    (Mtx lhs) + rhs = Mtx $ lhs + matrix (nrows lhs) (ncols lhs) (const rhs)
    lhs       + (Mtx rhs) = Mtx rhs + lhs

    -- multiply
    (Int lhs) * (Int rhs) = Int $ lhs * rhs
    (Flt lhs) * (Flt rhs) = Flt $ lhs * rhs
    (Cpx lhs) * (Cpx rhs) = Cpx $ lhs * rhs
    (Mtx lhs) * (Mtx rhs) =
        Mtx
        $   fromList (ncols lhs) (nrows lhs)
        $   getZipList
        $   (*)
        <$> lhs'
        <*> rhs'
      where
        lhs' = ZipList $ toList lhs
        rhs' = ZipList $ toList rhs

    (Flt lhs) * (Int rhs) = Flt lhs * Flt (fromIntegral rhs)
    (Int lhs) * (Flt rhs) = Flt (fromIntegral lhs) * Flt rhs

    (Int lhs) * (Cpx rhs) = (Cpx $ T.Complex (fromIntegral lhs, 0)) * Cpx rhs
    (Cpx lhs) * (Int rhs) = Cpx lhs * (Cpx $ T.Complex (fromIntegral rhs, 0))
    (Flt lhs) * (Cpx rhs) = (Cpx $ T.Complex (lhs, 0)) * Cpx rhs
    (Cpx lhs) * (Flt rhs) = Cpx lhs * (Cpx $ T.Complex (rhs, 0))

    (Mtx lhs) * rhs       = Mtx $ fmap (* rhs) lhs
    lhs       * (Mtx rhs) = Mtx rhs * lhs

    -- other
    negate (Int n) = Int $ negate n
    negate (Flt n) = Flt $ negate n
    negate (Cpx n) = Cpx $ negate n
    negate (Mtx n) = Mtx $ negate n

    abs (Int n) = Int $ abs n
    abs (Flt n) = Flt $ abs n
    abs (Cpx n) = Cpx $ abs n
    abs (Mtx n) = Mtx $ abs n

    signum (Int n) = Int $ signum n
    signum (Flt n) = Flt $ signum n
    signum (Cpx n) = Cpx $ signum n
    signum (Mtx n) = Mtx $ signum n

    fromInteger n = Int $ fromInteger n

showType :: BaseType -> String
showType (Int _) = "Int"
showType (Flt _) = "Float"
showType (Cpx _) = "Complex"
showType (Mtx _) = "Matrix"

-- returns `Left error message`
invalidInstruction :: Operator -> BaseType -> BaseType -> Either String BaseType
invalidInstruction op lhs rhs =
    Left
        $  "Invalid types for operation: "
        ++ showType lhs
        ++ " "
        ++ show op
        ++ " "
        ++ showType rhs

complexDiv :: Fractional a => T.Complex a -> T.Complex a -> T.Complex a
complexDiv (T.Complex (xa, xb)) (T.Complex (ya, yb)) = T.Complex (real, imag)
  where
    real = (xa * ya + xb * yb) / (ya * ya + yb * yb)
    imag = (xb * ya - xa * yb) / (ya * ya + yb * yb)

applyOp :: Operator -> BaseType -> BaseType -> Either String BaseType

-- mtx
applyOp Add (Mtx lhs) (Mtx rhs)
    | (ncols lhs == ncols rhs) && (nrows lhs == nrows rhs)
    = return $ Mtx $ lhs + rhs
    | otherwise
    = Left "Unable to add differently sized matrices"

applyOp MatrixMult (Mtx lhs) (Mtx rhs) = return $ Mtx $ lhs * rhs

-- all operations
applyOp Add        lhs       rhs       = return $ lhs + rhs
applyOp Sub        lhs       rhs       = return $ lhs - rhs
applyOp Mult       lhs       rhs       = return $ lhs * rhs

-- int
applyOp Div        (Int lhs) (Int rhs) = return $ Int $ lhs `div` rhs
applyOp Exp        (Int lhs) (Int rhs) = return $ Int $ lhs ^ rhs
applyOp Mod        (Int lhs) (Int rhs) = return $ Int $ lhs `mod` rhs

-- flt
applyOp Div        (Flt lhs) (Flt rhs) = return $ Flt $ lhs / rhs
applyOp Exp        (Flt lhs) (Flt rhs) = return $ Flt $ lhs ** rhs
applyOp op (Flt lhs) (Int rhs) = applyOp op (Flt lhs) (Flt (fromIntegral rhs))
applyOp op (Int lhs) (Flt rhs) = applyOp op (Flt (fromIntegral lhs)) (Flt rhs)

-- cpx
applyOp Div        (Cpx lhs) (Cpx rhs) = return $ Cpx $ lhs `complexDiv` rhs
applyOp Exp        (Cpx lhs) rhs       = invalidInstruction Exp (Cpx lhs) rhs
applyOp Exp        lhs       (Cpx rhs) = invalidInstruction Exp lhs (Cpx rhs)
applyOp op (Int lhs) (Cpx rhs) =
    applyOp op (Cpx $ T.Complex (fromIntegral lhs, 0)) (Cpx rhs)
applyOp op (Cpx lhs) (Int rhs) =
    applyOp op (Cpx lhs) (Cpx $ T.Complex (fromIntegral rhs, 0))
applyOp op (Flt lhs) (Cpx rhs) =
    applyOp op (Cpx $ T.Complex (lhs, 0)) (Cpx rhs)
applyOp op (Cpx lhs) (Flt rhs) =
    applyOp op (Cpx lhs) (Cpx $ T.Complex (rhs, 0))

applyOp op lhs rhs = invalidInstruction op lhs rhs
