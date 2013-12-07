module World where

import Data.Char

data World a = World { dim :: Pair, cells :: [[a]] }
    deriving Eq

type Pair = (Int, Int)

class (Eq l) => LiveCell l where
  isAlive :: l -> Bool
  isDead :: l -> Bool
  die :: l -> l
  born :: l -> l
  survive :: l -> l
  showText :: l -> Char
  getColour :: l -> (Float, Float, Float)
  deadC :: l
  newlC :: l

instance LiveCell a => Show (World a) where
  show = showWorld

showWorld :: LiveCell a => World a -> String
showWorld w = concatMap ((++ "\n") . (map showText)) (cells w)

instance LiveCell Bool where
  isAlive   = id
  isDead    = not
  die c     = False
  born c    = True
  survive c = True -- should make it better: c && True, equivalent to : c
  showText c | isAlive c = '#'
             | otherwise = ' '
  getColour c = (1,1,0)
  deadC     = False
  newlC     = True

instance LiveCell Int where
  isAlive c = c /= 0 && c <= maxAge
  isDead c  = c == 0 || c > maxAge
  die c     = 0
  born c    = 1
  survive c = c + 1  -- or: survive = (+1)
  showText c | isAlive c = chr (c - ord '0')
         | otherwise = ' '
  getColour c = (t,t,0)
    where
      t | c < maxAge = 1 - (fromIntegral c / fromIntegral maxAge)
        | otherwise  = 0
  deadC     = 0
  newlC     = 1

maxAge = 100

fullWorld :: Pair -> World Bool
fullWorld d = World d (replicate (fst d) (replicate (snd d) True))

emptyWorld :: Pair -> World Bool
emptyWorld d = World d (replicate (fst d) (replicate (snd d) False))

stillL :: World Bool
stillL = World (4,4) [replicate 4 False,
                      [False,True,True,False],
                      [False,True,True,False],
                      replicate 4 False]

blinker = World (3,3) [ [False,True,False] | x <- ["I will","go to","sleep now."] ]

blinkerAging = World (3,3) [ ([0,90,0] ::[Int])| x <- ["Aging","works","now!"] ]
