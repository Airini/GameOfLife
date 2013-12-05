module World where

import Data.Char

data World a = World { dim :: Pair, cells :: [[a]] }

type Pair = (Int, Int)

class LiveCell l where
  die :: l -> l
  born :: l -> l
  survive :: l -> l
  showText :: l -> Char
  getColour :: l -> (Float, Float, Float)
  maxAge :: l

instance LiveCell Bool where
  die c = False
  born c = True
  survive c = True
  showText c = if c
                  then '#'
		  else ' '
  getColour c = (1,1,0)
  maxAge = True


instance LiveCell Int where
  die c = 0
  born c = 1
  survive c = c + 1
  showText c = if c /= 0
                  then chr (c - ord '0')
		  else ' '
  getColour c = (t,t,0.0)
    where t | c < maxAge = 1.0 - (fromIntegral c) / (fromIntegral (maxAge::Int))
            | otherwise  = 0.0
  maxAge = 100

