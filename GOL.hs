module GOL where

import Test.QuickCheck
import Data.Maybe
import Data.List
import World

splitEv :: Eq a => Int -> [a] -> [[a]]
splitEv n l = map (\m -> take n (drop m l)) [0,n..length l] \\ [[]]

tickN :: LiveCell a => World a -> Int -> World a
tickN w n | n <= 0    = w
          | otherwise = tick $ tickN w (n-1)

tick :: (LiveCell a) => World a -> World a
tick w = World (dim w) rows
    where rows = splitEv xs (iterateCells w 0 0)
          iterateCells w' x y
              | y == ys    = []
              | x == xs-1  = updateCell w' x y : iterateCells w' 0     (y+1)
              | otherwise  = updateCell w' x y : iterateCells w' (x+1) y
          xs = fst (dim w)
          ys = snd (dim w)

-- TODO: add some comment about why cell with x and y is not needed to be
-- excluded.
updateCell :: LiveCell a => World a -> Int -> Int -> a
updateCell w x y | isAlive c && survival = survive c
                 | birth                 = born c
                 | otherwise             = die c
    where c = cells w !! y !! x
          nbrOfLivings = sum livingNeigh
          livingNeigh  = concatMap getNeigh neighRows
          getNeigh r   = map (neigh r) [xmin..xmax]
          neigh r x'   = if isAlive (r !! x') then 1 else 0
          neighRows    = map (cells w !!) [ymin..ymax]
          survival     = nbrOfLivings == 3 || nbrOfLivings == 4
          birth        = nbrOfLivings == 3
          xmin | x == 0             = 0
               | otherwise          = x-1
          xmax | x == fst (dim w)-1 = x
               | otherwise          = x+1
          ymin | y == 0             = 0
               | otherwise          = y-1
          ymax | y == snd (dim w)-1 = y
               | otherwise          = y+1
