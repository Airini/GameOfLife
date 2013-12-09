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
agedLife :: LiveCell a => World a -> World a -> Int -> Bool
agedLife w w' n = w' == (World (dim w) (map (map (ageCell n)) (cells w)))

ageCell :: LiveCell a => Int -> a -> a
ageCell n c | n <= 0    = c
            | isAlive c = survive (ageCell (n-1) c)
            | otherwise = deadC

tickN :: LiveCell a => World a -> Int -> World a
tickN w n | n <= 0    = w
          | otherwise = tick $ tickN w (n-1)

isTranslation :: LiveCell a => World a -> World a -> Bool
isTranslation v w = horzTransl t l || horzTransl l t ||
                    vertTransl t l || vertTransl l t ||
                    diagTransl t l || diagTransl l t
    where t = cells v
          l = cells w
          xs = fst $ dim v
          ys = snd $ dim v
          ds = min xs ys

horzTransl ([]:ts) _  = False
horzTransl as bs      = all (\(a,b) -> isPrefixOf a b) (zip as bs) ||
                        horzTransl (map tail as) bs
vertTransl []      _  = False
vertTransl (a:as)  bs = isPrefixOf (a:as) bs || vertTransl as bs
diagTransl _  _  = True

-----------------------------------------------------------------------------
{-= Generators and arbitrary instances =-}
cell :: LiveCell a => Gen a
cell = frequency [(17, return deadC), (2, rLiveC)]
    where rLiveC = do
                     age <- elements [1..ageScale]
                     return $ ageCell age newlC

instance LiveCell a => Arbitrary (World a) where
  arbitrary =
    do a <- arbitrary
       let d = mapTuple ((+1) . (`mod` 100)) a
       rows <- sequence [ sequence [ cell | i <- [1..fst d] ]
                                   | j <- [1..snd d]]
       return (World d rows)

-----------------------------------------------------------------------------
{-= Properties =-}
prop_wellFormedWorld :: LiveCell a => World a -> Bool
prop_wellFormedWorld w = length (cells w) == snd (dim w) &&
                         all validR (cells w)
    where validR s = (fst (dim w) == length s) &&
                     all (\c -> isAlive c || isDead c) s

-------
--- Might actually remove these since they are subsumed by the periodic tests
prop_stillLife :: LiveCell a => Int -> World a -> Property
prop_stillLife s w = w == tick w ==> w == tickN w reps
    where reps = mod s (div maxAge 2)

prop_eqStillLife :: LiveCell a => Int -> World a -> Property
prop_eqStillLife s w = equivLifeW w (tick w) ==> equivLifeW w (tickN w reps)
    where reps = mod s (div maxAge 2)

prop_agingStillLife :: LiveCell a => Int -> World a -> Property
prop_agingStillLife s w = equivLifeW w (tick w) ==> agedLife w (tickN w n) n
    where n = mod s (div maxAge 2)

-------

prop_oscillator :: LiveCell a => Int -> World a -> Int -> Property
prop_oscillator s w m = w == tickN w period ==> w == tickN w (2 * period)
    where period = mod m (div maxAge s)

prop_eqOscillator :: LiveCell a => Int -> World a -> Int -> Property
prop_eqOscillator s w m =
        equivLifeW w (tickN w period) ==> equivLifeW w (tickN w (2 * period))
    where period = mod m (div maxAge s)

prop_glider :: LiveCell a => Int -> World a -> Int -> Property
prop_glider = undefined

prop_spaceship :: LiveCell a => World a -> Property
prop_spaceship = undefined

-- /== checks valid tranitions between states
prop_updateCell :: World Int -> Bool
prop_updateCell w = all (\(x,y) -> ((cells w) !! y !! x) /== updateCell w x y) ps
    where ps = [(x,y) | x <- [0..dimX], y <- [0..dimY]]
          dimX = (fst . dim $ w) - 1
          dimY = (snd . dim $ w) - 1
          old /== new = (isDead  old && new == 1)   ||
                        (isDead  old && isDead new) ||
                        (isAlive old && isDead new) ||
                        (isAlive old && old == (new-1))

-------------------------------------------------------------------------
{-==-}

instance Arbitrary MapBlock where
  arbitrary =
    do offset <- elements [ (x,y) | x <- [(-20)..20], y <- [(-20)..20]]
       xs <- choose (1,10) :: Gen Int
       ys <- choose (1,10) :: Gen Int
       lives <- sequence [sequence [arbitrary | i <- [1..xs]] | j <- [1..ys]]
       return $ B offset lives

blockSet :: Gen [MapBlock]
blockSet = do
            n <- choose (1,8) :: Gen Int
            l <- sequence [arbitrary | i <- [1..n]]
            return l

--prop_readExpr e =  isJust e' &&  eval e == eval (fromJust e')
--  where e' = readExpr (showExpr e)

-------------------------------------------------------------------------
{-= From previous labs =-}

-- QuickCheck helper: allows determining number of examples to check
numberChecks n = quickCheckWith stdArgs{ maxSuccess = n }


