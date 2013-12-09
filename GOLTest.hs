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

prop_stillLife :: LiveCell a => Int -> World a -> Property
prop_stillLife s w = w == tick w ==> w == tickN w reps
    where reps = mod s (div maxAge 2)

prop_eqStillLife :: LiveCell a => Int -> World a -> Property
prop_eqStillLife s w = equivLifeW w (tick w) ==> equivLifeW w (tickN w reps)
    where reps = mod s (div maxAge 2)

prop_agingStillLife :: LiveCell a => In
prop_periodicTicks :: LiveCell a => Int -> World a -> Int -> Property
prop_periodicTicks s w m = w == tickN w period ==> w == tickN w (2 * period)
    where period = mod m (div maxAge s)

prop_eqPeriodicTicks :: LiveCell a => Int -> World a -> Int -> Property
prop_eqPeriodicTicks s w m =
        equivLifeW w (tickN w period) ==> equivLifeW w (tickN w (2 * period))
    where period = mod m (div maxAge s)

prop_guns :: LiveCell a => Int -> World a -> Int -> Property
prop_guns = undefined

-------------------------------------------------------------------------

-- QuickCheck helper: allows determining number of examples to check
numberChecks n = quickCheckWith stdArgs{ maxSuccess = n }



