module Matrix
    ( Matrix
    , nrows
    , ncols
    , fromList
    , fromLists
    , toList
    , matrix
    , elementwise
    )
where

import           Data.List
import           Control.Applicative

data Matrix a = M { nrows :: Int
                  , ncols :: Int
                  , getBody :: [[a]]
                  } deriving (Eq)

-- construct

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix nr nc f = fromList nr nc $ curry f <$> [1 .. nr] <*> [1 .. nc]

fromlst :: Int -> Int -> [a] -> [[a]]
fromlst 0  _  _  = []
fromlst nr nc xs = take nc xs : fromlst (nr - 1) nc (drop nc xs)

fromList :: Int -> Int -> [a] -> Matrix a
fromList nr nc = M nr nc . fromlst nr nc

fromLists :: [[a]] -> Matrix a
fromLists []         = error "fromLists: empty list."
fromLists [[]      ] = M 0 0 []
fromLists (xs : xss) = fromList n m $ concat $ xs : fmap (take m) xss
  where
    n = 1 + length xss
    m = length xs

-- access

toList :: Matrix a -> [a]
toList = concat . getBody

mtxIdx :: Matrix a -> (Int, Int) -> a
mtxIdx (M _ _ xs) (r, c) = xs !! (r - 1) !! (c - 1)

getCol :: Int -> Matrix a -> [a]
getCol n xs | n > ncols xs = error "getCol index too large"
            | otherwise    = map (!! (n - 1)) (getBody xs)

getRow :: Int -> Matrix a -> [a]
getRow n xs | n > nrows xs = error "getRow index too large"
            | otherwise    = getBody xs !! (n - 1)

-- mapping/function application

elementwise :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
elementwise f m m' =
    matrix (nrows m) (ncols m) $ \k -> f (mtxIdx m k) (mtxIdx m' k)

instance Functor Matrix where
    fmap f (M nr nc xs) = M nr nc $ fmap (fmap f) xs

-- math

solveIdx :: Num a => Matrix a -> Matrix a -> (Int, Int) -> a
solveIdx m n (r, c) =
    let rm = ZipList $ getRow r m
        cn = ZipList $ getCol c n
    in  sum $ getZipList $ (*) <$> rm <*> cn

mtxMult :: Num a => Matrix a -> Matrix a -> Matrix a
mtxMult m n =
    fromList (nrows m) (ncols n)
        $   map (solveIdx m n)
        $   (,)
        <$> [1 .. nrows m]
        <*> [1 .. ncols n]

instance Num a => Num (Matrix a) where
    fromInteger n = M 1 1 [[fromInteger n]]
    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+)    = elementwise (+)
    (-)    = elementwise (-)
    (*)    = mtxMult

-- show

instance Show a => Show (Matrix a) where
    show m = "[" ++  (intercalate "; " $ map show $ getBody m) ++ "]"
