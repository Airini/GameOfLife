module World where

import Data.Char

-- Class for cells in a World for GOL
class (Eq l) => LiveCell l where
  -- cell states
  isAlive :: l -> Bool
  isDead  :: l -> Bool
  eqLiveness :: l -> l -> Bool
  eqLiveness a b = isAlive a == isAlive b
  -- transitions
  die     :: l -> l
  born    :: l -> l
  survive :: l -> l
  -- constants for newly created cells
  deadC :: l
  newlC :: l
  -- visual representations
  showText :: l -> String
  getColour :: l -> (Float, Float, Float)

type Pair = (Int, Int)

data World a = World { dim :: Pair, cells :: [[a]] }
    deriving Eq

-- Determines if two worlds are equivalent with respect to the liveness
-- state of their cells
equivLifeW :: LiveCell a => World a -> World a -> Bool
equivLifeW v w = dim v == dim w &&
                 all (uncurry eqLiveness)
                     (concatMap zipCols zippedRows)
    where zippedRows = zip (cells v) (cells w)
          zipCols    = uncurry zip

instance LiveCell a => Show (World a) where
  show = showWorld

showWorld :: LiveCell a => World a -> String
showWorld w = concatMap ((++ "\n") . concatMap showText) (cells w)

-- Given a base representation where liveness is indicated by boolean values,
-- a World of an appropriate LiveCell type of cells is constructed
fillCells :: LiveCell a => World Bool -> World a
fillCells (World d cs) =
    World d (map (map (\b -> if b then newlC else deadC)) cs)


-----------------------------------------------------------------------------
{-= Type instances for GOL world cells =-}

instance LiveCell Bool where
  isAlive   = id
  isDead    = not
  die c     = False
  born c    = True
  survive c = True
  showText c | isAlive c = "#"
             | otherwise = "."
  getColour c = (1,1,0)
  deadC     = False
  newlC     = True

instance LiveCell Int where
  isAlive c = c /= 0 && c <= maxAge
  isDead c  = c == 0 || c > maxAge
  die c     = 0
  born c    = 1
  survive c = c + 1
  showText c = replicate (prefix c') ' ' ++ t
    where c' | isAlive c = c
             | otherwise = 1
          t | isAlive c = show c
            | otherwise = "."
  getColour c = (t,t,0)
    where
      t | c < maxAge = 1 - (fromIntegral c / fromIntegral maxAge)
        | otherwise  = 0
  deadC     = 0
  newlC     = 1

-- Gives an upper bound for the set of ages
maxAge = 100

-- Figures out the prefix space padding for the display for Int cells
prefix :: Int -> Int
prefix n = order maxAge - order n + 1

-- Calculates the order of magnitude of an integer in powers of 10
order :: Int -> Int
order 0 = 0
order n = 1 + order (div n' 10)
    where n' = abs n

---------
-- Specific world creators

fullWorld :: LiveCell a => Pair -> World a
fullWorld d = World d (replicate (snd d) (replicate (fst d) newlC))

emptyWorld :: LiveCell a => Pair -> World a
emptyWorld d = World d (replicate (snd d) (replicate (fst d) deadC))

stillL :: World Bool
stillL = World (4,4) [replicate 4 False,
                      [False,True,True,False],
                      [False,True,True,False],
                      replicate 4 False]

blinker = World (3,3) [[False,True,False] | x <- [1..]]

blinkerAging = World (3,3) [[0,1,0] ::[Int] | x <- [1..]]

