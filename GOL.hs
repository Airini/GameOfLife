import Test.QuickCheck
import Data.Maybe
import Data.List
--import Data.Char

data World = World { dim :: Pair, cells :: [[Bool]] }
    deriving (Eq, Show)

type Pair = (Int, Int)

emptyWorld :: Pair -> World--[[Bool]]-- -> World
emptyWorld d = World d (replicate (fst d) (replicate (snd d) False))

printWorld :: World -> IO ()
printWorld w = mapM_ (print . cols) (cells w)
    where cols = map (\c -> if c
                               then '#'
                               else ' ')


main :: IO ()
main = do
         let initW = emptyWorld (20,20)
         runGame initW

runGame :: World -> IO ()
runGame w | w == nextW  = printWorld w
          | otherwise   = do
                            printWorld w
                            runGame nextW
    where nextW = w-- tick ((fst . dim w)) ((snd . dim w))

splitEv :: Eq a => Int -> [a] -> [[a]]
splitEv n l = map (\m -> take n (drop m l)) [0,n..length l] \\ [[]]

tick :: World -> World
tick w = World (dim w) rows
    where rows = splitEv xs (iterateCells w 0 0)
          iterateCells w' x y
              | y == ys    = []
              | x == xs-1  = (updateCell w' x y) : iterateCells w' 0     (y+1)
              | otherwise  = (updateCell w' x y) : iterateCells w' (x+1) y
          xs = fst (dim w)
          ys = snd (dim w)

updateCell :: World -> Int -> Int -> Bool
updateCell w x y | ((cells w) !! y) !! x = survival
                 | otherwise             = birth
    where neighbours =
              sum $ concatMap
                (\ys' -> (map (\x' -> if (ys' !! x') then 1 else 0)
                              [xmin..xmax]))
                neighY
          neighY   = map (cells w !!) [ymin..ymax]
          survival = neighbours == 3 || neighbours == 4
          birth    = neighbours == 3
          xmin | x == 0    = 0
               | otherwise = x-1
          xmax | x == (fst (dim w))-1 = x
               | otherwise            = x+1
          ymin | y == 0    = 0
               | otherwise = y-1
          ymax | y == (snd (dim w))-1 = y
               | otherwise           = y+1

{-dumm :: [Int] -> int -> Int -> [Int]
dumm l 0 _ = [head l]
dumm l t n = []
  where t = n
dumm l a n = (l !! n) : (dumm l (a-1) n)
-}

