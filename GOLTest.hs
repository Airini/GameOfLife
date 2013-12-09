module GOLTest where

import Test.QuickCheck
import World
import GOL
import ReadGOL

cell :: LiveCell a => Gen a
cell = frequency [(13, return deadC), (2, return newlC)]

instance LiveCell a => Arbitrary (World a) where
  arbitrary =
    do a <- arbitrary
       let d = mapTuple ((+1) . (`mod` 200)) a
       rows <- sequence [ sequence [ cell | i <- [1..fst d] ]
                                   | j <- [1..snd d]]
       return (World d rows)

prop_wellFormedWorld :: LiveCell a => World a -> Bool
prop_wellFormedWorld w = length (cells w) == snd (dim w) &&
                         all validR (cells w)
    where validR s = (fst (dim w) == length s) &&
                     all (\c -> isAlive c || isDead c) s

prop_periodicTicks :: LiveCell a => Int -> World a -> Int -> Property
prop_periodicTicks s w m = w == tickN w period ==> w == tickN w (2 * period)
    where tickN w 0 = w
          tickN w n = tick $ tickN w (n-1)
          period = mod m (div maxAge s)

prop_guns :: LiveCell a => Int -> World a -> Int -> Property
prop_guns = undefined

-------------------------------------------------------------------------

-- QuickCheck helper: allows determining number of examples to check
numberChecks n = quickCheckWith stdArgs{ maxSuccess = n }





