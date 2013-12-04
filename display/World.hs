module World where

type Pair = (Int, Int)
data World = World { dim :: Pair, cells :: [[Bool]] }
           deriving (Eq, Show)

emptyWorld :: Pair -> World
emptyWorld d = World d (replicate (fst d) (replicate (snd d) True))
