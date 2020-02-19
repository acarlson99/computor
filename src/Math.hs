module Math
    ( BaseType(..)
    , applyOp
    )
where

import qualified Complex                       as C
import           Matrix

import           Parse.Types

data BaseType = Int Int
              | Flt Float
              | Cpx (C.Complex Float)
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

    (Int lhs) + (Cpx rhs) = (Cpx $ C.Complex (fromIntegral lhs, 0)) + Cpx rhs
    (Cpx lhs) + (Int rhs) = Cpx lhs + (Cpx $ C.Complex (fromIntegral rhs, 0))
    (Flt lhs) + (Cpx rhs) = (Cpx $ C.Complex (lhs, 0)) + Cpx rhs
    (Cpx lhs) + (Flt rhs) = Cpx lhs + (Cpx $ C.Complex (rhs, 0))

    (Mtx lhs) + rhs = Mtx $ lhs + matrix (nrows lhs) (ncols lhs) (const rhs)
    lhs       + (Mtx rhs) = Mtx rhs + lhs

    -- multiply
    (Int lhs) * (Int rhs) = Int $ lhs * rhs
    (Flt lhs) * (Flt rhs) = Flt $ lhs * rhs
    (Cpx lhs) * (Cpx rhs) = Cpx $ lhs * rhs
    (Mtx lhs) * (Mtx rhs) = Mtx $ elementwise (*) lhs rhs

    (Flt lhs) * (Int rhs) = Flt lhs * Flt (fromIntegral rhs)
    (Int lhs) * (Flt rhs) = Flt (fromIntegral lhs) * Flt rhs

    (Int lhs) * (Cpx rhs) = (Cpx $ C.Complex (fromIntegral lhs, 0)) * Cpx rhs
    (Cpx lhs) * (Int rhs) = Cpx lhs * (Cpx $ C.Complex (fromIntegral rhs, 0))
    (Flt lhs) * (Cpx rhs) = (Cpx $ C.Complex (lhs, 0)) * Cpx rhs
    (Cpx lhs) * (Flt rhs) = Cpx lhs * (Cpx $ C.Complex (rhs, 0))

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

invalidTypes :: Operator -> BaseType -> BaseType -> String
invalidTypes op lhs rhs =
    "Invalid types for operation: `"
        ++ showType lhs
        ++ " "
        ++ show op
        ++ " "
        ++ showType rhs
        ++ "`"

invalidParameters :: Operator -> BaseType -> BaseType -> String
invalidParameters op lhs rhs =
    "Invalid parameters for operation: `"
        ++ show lhs
        ++ " "
        ++ show op
        ++ " "
        ++ show rhs
        ++ "`"

complexDiv :: Fractional a => C.Complex a -> C.Complex a -> C.Complex a
complexDiv (C.Complex (xa, xb)) (C.Complex (ya, yb)) = C.Complex (real, imag)
  where
    real = (xa * ya + xb * yb) / (ya * ya + yb * yb)
    imag = (xb * ya - xa * yb) / (ya * ya + yb * yb)

checkZero :: Eq b => a -> b -> b -> Either a b
checkZero msg var z | var == z  = Left msg
                    | otherwise = return var

applyOp :: Operator -> BaseType -> BaseType -> Either String BaseType
-- mtx dimension check
applyOp Add (Mtx lhs) (Mtx rhs)
    | (ncols lhs == ncols rhs) && (nrows lhs == nrows rhs)
    = return $ Mtx $ lhs + rhs
    | otherwise
    = Left
        $  "Unable to add differently sized matrices: "
        ++ show (ncols lhs, nrows lhs)
        ++ " "
        ++ show (ncols rhs, nrows rhs)
applyOp Sub (Mtx lhs) (Mtx rhs)
    | (ncols lhs == ncols rhs) && (nrows lhs == nrows rhs)
    = return $ Mtx $ lhs - rhs
    | otherwise
    = Left
        $  "Unable to subtract differently sized matrices: "
        ++ show (ncols lhs, nrows lhs)
        ++ " "
        ++ show (ncols rhs, nrows rhs)
applyOp Mult (Mtx lhs) (Mtx rhs)
    | (ncols lhs == ncols rhs) && (nrows lhs == nrows rhs)
    = return $ Mtx lhs * Mtx rhs
    | otherwise
    = Left
        $  "Unable to multiply differently sized matrices: "
        ++ show (ncols lhs, nrows lhs)
        ++ " "
        ++ show (ncols rhs, nrows rhs)
applyOp MatrixMult (Mtx lhs) (Mtx rhs)
    | ncols lhs == nrows rhs
    = return $ Mtx $ lhs * rhs
    | otherwise
    = Left
        $  "Unable to multiply unbalanced matrices.  cols lhs /= rows rhs: "
        ++ show (ncols lhs, nrows lhs)
        ++ " "
        ++ show (ncols rhs, nrows rhs)

-- normal operations
applyOp Add  lhs       rhs       = return $ lhs + rhs
applyOp Sub  lhs       rhs       = return $ lhs - rhs
applyOp Mult lhs       rhs       = return $ lhs * rhs

-- int
applyOp Div  (Int lhs) (Int rhs) = Int . div lhs <$> checkZero
    (invalidParameters Div (Int lhs) (Int rhs) ++ " divisor must be non-zero")
    rhs
    0
applyOp Exp (Int lhs) (Int rhs)
    | rhs < 0
    = Left
        $  invalidParameters Exp (Int lhs) (Int rhs)
        ++ " exponent must be non-negative"
    | otherwise
    = return $ Int $ lhs ^ rhs
applyOp Mod (Int lhs) (Int rhs) = Int . mod lhs <$> checkZero
    (invalidParameters Mod (Int lhs) (Int rhs) ++ " divisor must be non-zero")
    rhs
    0

-- flt
applyOp Div (Flt lhs) (Flt rhs) = Flt . (lhs /) <$> checkZero
    (invalidParameters Div (Flt lhs) (Flt rhs) ++ " divisor must be non-zero")
    rhs
    0
applyOp Exp (Flt lhs) (Flt rhs)
    | rhs < 0
    = Left
        $  invalidParameters Exp (Flt lhs) (Flt rhs)
        ++ " exponent must be non-negative"
    | otherwise
    = return $ Flt $ lhs ** rhs
applyOp op  (Flt lhs) (Int rhs) = applyOp op (Flt lhs) (Flt (fromIntegral rhs))
applyOp op  (Int lhs) (Flt rhs) = applyOp op (Flt (fromIntegral lhs)) (Flt rhs)

-- cpx
applyOp Div (Cpx lhs) (Cpx rhs) = Cpx . complexDiv lhs <$> checkZero
    (invalidParameters Div (Cpx lhs) (Cpx rhs) ++ " divisor must be non-zero")
    rhs
    0
applyOp Exp (Cpx lhs) rhs       = Left $ invalidTypes Exp (Cpx lhs) rhs
applyOp Exp lhs       (Cpx rhs) = Left $ invalidTypes Exp lhs (Cpx rhs)
applyOp op (Int lhs) (Cpx rhs) =
    applyOp op (Cpx $ C.Complex (fromIntegral lhs, 0)) (Cpx rhs)
applyOp op (Cpx lhs) (Int rhs) =
    applyOp op (Cpx lhs) (Cpx $ C.Complex (fromIntegral rhs, 0))
applyOp op (Flt lhs) (Cpx rhs) =
    applyOp op (Cpx $ C.Complex (lhs, 0)) (Cpx rhs)
applyOp op (Cpx lhs) (Flt rhs) =
    applyOp op (Cpx lhs) (Cpx $ C.Complex (rhs, 0))

applyOp op lhs rhs = Left $ invalidTypes op lhs rhs
