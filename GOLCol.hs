module GOLCol where

import Test.QuickCheck
import Data.Maybe
import Data.List
import Data.Char


data World = World { dim :: Pair, cells :: [[Int]] }
    deriving (Eq, Show)

type Pair = (Int, Int)

emptyWorld :: Pair -> World
emptyWorld d = World d (replicate (fst d) (replicate (snd d) 0))

printPlainWorld :: World -> IO ()
printPlainWorld w = mapM_ (putStrLn . cols) (cells w)
    where cols = map (\c -> if c /= 0
                               then '#'
                               else ' ')

printAgingWorld :: World -> IO ()
printAgingWorld w = mapM_ (print . cols) (cells w)
    where cols = map (\c -> if c /= 0
                               then chr (c + ord '0')
                               else ' ')


main :: IO ()
main = do
         let initW = emptyWorld (20,20)
         runGame initW

runGame :: World -> IO ()
runGame w | w == nextW  = printAgingWorld w
          | otherwise   = do
                            printAgingWorld w
                            runGame nextW
    where nextW = tick w

splitEv :: Eq a => Int -> [a] -> [[a]]
splitEv n l = map (\m -> take n (drop m l)) [0,n..length l] \\ [[]]

tick :: World -> World
tick w = World (dim w) rows
    where rows = splitEv xs (iterateCells w 0 0)
          iterateCells w' x y
              | y == ys    = []
              | x == xs-1  = updateCell w' x y : iterateCells w' 0     (y+1)
              | otherwise  = updateCell w' x y : iterateCells w' (x+1) y
          xs = fst (dim w)
          ys = snd (dim w)

stillL :: World
stillL = World (4,4) [replicate 4 0,
                      [0,1,1,0],
                      [0,1,1,0],
                      replicate 4 0]

-- TODO: add some comment about why cell with x and y is not needed to be
-- excluded.
updateCell :: World -> Int -> Int -> Int
updateCell w x y | 0 /= cells w !! y !! x = survival
                 | otherwise              = birth
    where nbrOfLivings = sum livingNeigh
          livingNeigh  = concatMap getNeigh neighRows
          getNeigh r   = map (neigh r) [xmin..xmax]
          neigh r x'   = if (r !! x') /= 0 then 1 else 0
          neighRows    = map (cells w !!) [ymin..ymax]
          survival     = if nbrOfLivings == 3 || nbrOfLivings == 4
                            then 1 + cells w !! y !! x
                            else 0
          birth        = if nbrOfLivings == 3
                            then 1
                            else 0
          xmin | x == 0             = 0
               | otherwise          = x-1
          xmax | x == fst (dim w)-1 = x
               | otherwise          = x+1
          ymin | y == 0             = 0
               | otherwise          = y-1
          ymax | y == snd (dim w)-1 = y
               | otherwise          = y+1


