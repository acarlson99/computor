module Matrix
    ( Matrix
    , nrows
    , ncols
    , fromList
    , fromLists
    , toList
    , matrix
    -- ~ , matrixGet
    , elementwise
    -- ~ , prettyMatrix
    )
where

data Matrix a = M { nrows :: Int
                  , ncols :: Int
                  , getBody :: [[a]]
                  } deriving (Eq)

-- TODO: fix formatting
prettyMatrix :: Show a => Matrix a -> String
prettyMatrix m = concat
    [ "┌ "
    , unwords (replicate (ncols m) blank)
    , " ┐\n"
    , unlines
        [ "│ "
          ++ unwords
                 (fmap (\j -> fill $ strings `matrixGet` (i, j)) [1 .. ncols m])
          ++ " │"
        | i <- [1 .. nrows m]
        ]
    , "└ "
    , unwords (replicate (ncols m) blank)
    , " ┘"
    ]
  where
    strings@(M _ _ v) = fmap show m
    widest            = foldr max minBound $ fmap length v
    fill str = replicate (widest - length str) ' ' ++ str
    blank = fill ""

instance Show a => Show (Matrix a) where
    show = prettyMatrix

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix nr nc f = fromList nr nc $ (curry f) <$> [1 .. nr] <*> [1 .. nc]

fromlst :: Int -> Int -> [a] -> [[a]]
fromlst 0  _  _  = []
fromlst nr nc xs = take nc xs : fromlst (nr - 1) nc (drop nc xs)

fromList :: Int -> Int -> [a] -> Matrix a
fromList nr nc = M nr nc . fromlst nr nc

toList :: Matrix a -> [a]
toList = concat . getBody

matrixGet :: Matrix a -> (Int, Int) -> a
matrixGet (M _ _ xs) (r, c) = xs !! (r - 1) !! (c - 1)

elementwise :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
elementwise f m m' =
    matrix (nrows m) (ncols m) $ \k -> f (matrixGet m k) (matrixGet m' k)

fromLists :: [[a]] -> Matrix a
fromLists []         = error "fromLists: empty list."
fromLists [[]      ] = M 0 0 []
fromLists (xs : xss) = fromList n m $ concat $ xs : fmap (take m) xss
  where
    n = 1 + length xss
    m = length xs

instance Functor Matrix where
    fmap f (M nr nc xs) = M nr nc $ fmap (fmap f) xs

instance Num a => Num (Matrix a) where
    fromInteger n = M 1 1 [[fromInteger n]]
    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+)    = elementwise (+)
    (-)    = elementwise (-)
    -- ~ (*)    = elementwise (*)
    (*)    = undefined
