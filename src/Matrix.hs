module Matrix
    ( Matrix(..)
    )
where

data Matrix a = M { nrows :: Int
                  , ncols :: Int
                  , body :: [[a]]
                  } deriving (Show)
