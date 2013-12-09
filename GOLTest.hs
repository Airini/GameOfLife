module GOLTest where

import Test.QuickCheck
import World
import GOL
import ReadGOL

cell :: LiveCell a => Gen a
cell = frequency [(9, return deadC), (2, return newlC)]

instance LiveCell a => Arbitrary (World a) where
  arbitrary =
    do a <- arbitrary
       let d = mapTuple ((+1) . (`mod` 200)) a
       rows <- sequence [ sequence [ cell | i <- [1..fst d] ]
                                   | j <- [1..snd d]]
       return (World d rows)

