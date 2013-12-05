module World where

import Data.Char

data World a = World { dim :: Pair, cells :: [[a]] }
    deriving (Eq)

type Pair = (Int, Int)

class (Eq l) => LiveCell l where
  isAlive :: l -> Bool
  isDead :: l -> Bool
  die :: l -> l
  born :: l -> l
  survive :: l -> l
  showText :: l -> Char
  getColour :: l -> (Float, Float, Float)
  maxAge :: l

instance LiveCell Bool where
  isAlive c = c
  isDead c  = not c
  die c     = False
  born c    = True
  survive c = True
  showText c | isAlive c = '#'
             | otherwise = ' '
  getColour c = (1,1,0)
  maxAge    = True


instance LiveCell Int where
  isAlive c = c /= 0
  isDead c  = c == 0
  die c     = 0
  born c    = 1
  survive c = c + 1
  showText c | isAlive c = chr (c - ord '0')
	     | otherwise = ' '
  getColour c = (t,t,0.0)
    where t | c < maxAge = 1.0 - (fromIntegral c) / (fromIntegral (maxAge::Int))
            | otherwise  = 0.0
  maxAge = 100

