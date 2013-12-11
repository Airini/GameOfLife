module GOLTest where

import Test.QuickCheck
import World
import GOL
import ReadGOL
import Data.List

-- Constant to determine how old a cell can initally be in testing, scaled
-- from the maximum age for a cell given the modified behaviour with respect
-- to standard GOL rules of cells aging past that limit
ageScale = div maxAge 5

-----------------------------------------------------------------------------
{-= Helper functions that deal with the pure structure (ie: not arbitary) =-}
  -- These function are to be used locally in the testing module and hence
  -- are assumed to be called with appropriate parameters, eg: positive
  -- number of ticks/generations, comparable worlds (ie: of same dimension)

-- Given two worlds, determines if the second one is identical to the first
-- after aging all cells by as many ticks as parameter n states
agedLife :: LiveCell a => World a -> World a -> Int -> Bool
agedLife w w' n = w' == World (dim w) (map (map (ageCell n)) (cells w))

-- Produces the resulting cell of aging a cell by n ticks
ageCell :: LiveCell a => Int -> a -> a
ageCell n c | n <= 0    = c
            | isAlive c = survive (ageCell (n-1) c)
            | otherwise = deadC

-- Ticks a world n times
tickN :: LiveCell a => World a -> Int -> World a
tickN w n | n <= 0    = w
          | otherwise = tick $ tickN w (n-1)


-- ** --> Unfinished block: helper functions for glider and spaceship checks
--    NOTE: the functions which are actually implemented in this block have
--          not been tested properly

-- Checks if a world is a translation of another
isTranslation :: LiveCell a => World a -> World a -> Bool
isTranslation v w = horzTransl t l || horzTransl l t ||
                    vertTransl t l || vertTransl l t ||
                    diagTransl t l || diagTransl l t
    where t = cells v
          l = cells w
          xs = fst $ dim v
          ys = snd $ dim v
          ds = min xs ys

-- horizontal translation
horzTransl ([]:ts) _  = False
horzTransl as bs      = all (uncurry isPrefixOf) (zip as bs) ||
                        horzTransl (map tail as) bs
-- vertical translation
vertTransl []      _  = False
vertTransl (a:as)  bs = isPrefixOf (a:as) bs || vertTransl as bs
-- diagonal translation
diagTransl _  _  = True
-- ** --> here ends the unfinished block

-----------------------------------------------------------------------------
{-= Generators and arbitrary instances =-}

-- Individual cell generator
cell :: LiveCell a => Gen a
cell = frequency [(27, return deadC), (2, rLiveC)]
    where rLiveC = do
                     age <- elements [1..ageScale]
                     return $ ageCell age newlC

-- World arbitrary instantiation
instance LiveCell a => Arbitrary (World a) where
  arbitrary =
    do a <- arbitrary
       let d = mapTuple ((+1) . (`mod` 100)) a
       rows <- sequence [ sequence [ cell | i <- [1..fst d] ]
                                   | j <- [1..snd d]]
       return (World d rows)

-- Specific generator for worlds which oscillate with a period n
oscillatingWorld :: LiveCell a => Int -> Gen (World a)
oscillatingWorld n =
    do
      w <- arbitrary
      if equivLifeW w (tickN w n)
         then return w
         else oscillatingWorld n

{- ** --> Unfinished work: generator for gliders and spaceships
   The (approximate) idea was to generate an arbitrary world, iterate it by
   UP TO (so all intermediate states are checked, in an attempt to shorten
   number of worlds one would have to generate until encountering an
   appropriate one) a number of ticks (indicated by a parameter) and check if
   the same pattern is detected somewhere other in the board (ie: after a
   translation)
-- ** --> here ends the unfinished block -}

-----------------------------------------------------------------------------
{-= Properties =-}

-- Property got check arbitrary worlds are generated correctly
-- NB: requires type definition when quickCheck'ed because of the sole
--     specification of the class of LiveCell but not the actual type
prop_wellFormedWorld :: LiveCell a => World a -> Bool
prop_wellFormedWorld w = length (cells w) == snd (dim w) &&
                         all validR (cells w)
    where validR s = (fst (dim w) == length s) &&
                     all (\c -> isAlive c || isDead c) s

---------
--- The following properties on still lives are subsumed by oscillators
--- Included nonetheless for incremental testing
--- Specific tests for the two LiveCell class types implemented

prop_stillB :: Int -> Property
prop_stillB s = forAll (oscillatingWorld 1 :: Gen (World Bool))
                     $ \w -> w == tickN w reps
    where reps = mod s (div maxAge 2)

prop_stillI :: Int -> Property
prop_stillI s = forAll (oscillatingWorld 1 :: Gen (World Int))
                     $ \w -> equivLifeW w (tickN w reps)
    where reps = mod s (div maxAge 2)

-- Unlike the previous simple check for same liveness state in Int worlds,
-- this property checks a still world ages correctly (in terms of the number
-- of ticks lived by each cell
prop_agingStill :: Int -> Property
prop_agingStill s = forAll (oscillatingWorld 1 :: Gen (World Int))
                         $ \w -> agedLife w (tickN w n) n
    where n = mod s (div maxAge 2)

---------
-- Properties on correctnes of world evolution for oscillators
prop_oscB :: Int -> Property
prop_oscB m = forAll (oscillatingWorld period :: Gen (World Bool))
                   $ \w -> w == tickN w (2 * period)
    where period = mod m (div maxAge ageScale)

prop_oscI :: Int -> Property
prop_oscI m = forAll (oscillatingWorld period :: Gen (World Int))
                   $ \w -> equivLifeW w (tickN w (2 * period))
    where period = mod m (div maxAge ageScale)

-- ** --> Unfinished work: properties for gliders and spaceships
prop_glider :: LiveCell a => Int -> World a -> Int -> Property
prop_glider = undefined

prop_spaceship :: LiveCell a => World a -> Property
prop_spaceship = undefined
-- ** --> here ends the unfinished block

-- /== checks valid transitions between states
prop_updateCell :: World Int -> Bool
prop_updateCell w = all (\(x,y) -> (cells w !! y !! x) /== updateCell w x y) ps
    where ps = [(x,y) | x <- [0..dimX], y <- [0..dimY]]
          dimX = (fst . dim $ w) - 1
          dimY = (snd . dim $ w) - 1
          old /== new = (isDead  old && new == 1)   ||
                        (isDead  old && isDead new) ||
                        (isAlive old && isDead new) ||
                        (isAlive old && old == (new-1))

-----------------------------------------------------------------------------
{-= Section for parser testing =-}

-- Arbitrary instance for types of blocks read in input files
instance Arbitrary MapBlock where
  arbitrary =
    do offset <- elements [ (x,y) | x <- [(-20)..20], y <- [(-20)..20]]
       xs <- choose (1,10) :: Gen Int
       ys <- choose (1,10) :: Gen Int
       lives <- sequence [sequence [arbitrary | i <- [1..xs]] | j <- [1..ys]]
       return $ B offset lives

-- Generator for a list of map blocks
blockSet :: Gen [MapBlock]
blockSet = do
            n <- choose (1,8) :: Gen Int
            sequence [arbitrary | i <- [1..n]]

{- ** --> Unfinished block: parser testing
   The idea was to create a function to turn a list of MapBlock into an
   equivalent text representation in the format read (functions for turning
   world maps into text haven't been created) and then read it.
   The property would check: worldify (in module ReadGOL) applied on both the
   arbitrary list of MapBlock generated and the parsed list of MapBlock from
   its text representation gives the same world.
-  ** --> end of unfinished block -}

-----------------------------------------------------------------------------
{-= From previous labs =-}

-- QuickCheck helper: allows determining number of examples to check
numberChecks n = quickCheckWith stdArgs{ maxSuccess = n }


