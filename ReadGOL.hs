module ReadGOL where

import World
import Parsing
import Data.Char
import Data.Maybe

-----------------------------------------------------------------------------
{- Adapted from ReadExprMonadic -}
-- Natural number parser
nat :: Parser Int
nat = do ds <- oneOrMore digit
         return (read ds)

-- Integer parser
integer :: Parser Int 
integer = nat +++ neg -- natural or negative
  where neg = do char '-'
                 n <- nat
                 return $ negate n
-----------------------------------------------------------------------------


-- Parses using a first parser until an occurrence parsable by a seond
-- parser is encountered, ie: it is equivalent to "parse a until b is parsed"
--  NOTE: uses _peak_ which we added to the _Parsing_ module since it
--        employs the definition of the _Parser_ constructor, not exported
(>-|) :: Parser a -> Parser b -> Parser [a]
p >-| q = (peak q >-> return []) +++ (p <:> (p >-| q))

-- Parses a constant string (eg: a specification reserved word)
specString :: String -> Parser String
specString []     = failure
specString [c]    = pmap (:[]) (char c)
specString (c:cs) = char c <:> specString cs

-- Parser which ignores a line returning a default argument value
ignore :: a -> Parser a
ignore t = zeroOrMore (sat (/= '\n')) >-> char '\n' >-> return t


-- Parses an offset (in the world map) pair with respect to the origin (0,0)
offset :: Parser Pair
offset = specString "#P" >-> oneOrMore (sat isSpace) >->
         (integer >*> (\y -> oneOrMore (sat isSpace) >->
                             integer >*> \x -> success (x,y))) <-< char '\n'

offset' :: Parser Pair
offset' = specString "#P" >-> oneOrMore (sat isSpace) >->
          do
            y <- integer
            x <- oneOrMore (sat isSpace) >-> integer <-< char '\n'
            return (x,y)

offset'' :: Parser Pair
offset'' = do
             specString "#P"
             oneOrMore (sat isSpace)
             y <- integer
             oneOrMore (sat isSpace)
             x <- integer
             char '\n'
             return (x,y)


-- Parses a dead cell
deadCell :: Parser Bool
deadCell = char '.' >-> success False

-- Parses a living cell
liveCell :: Parser Bool
liveCell = char '*' >-> success True

-- Parses a row of a map made up solely of dead and living cells
plainRow :: Parser [Bool]
plainRow = oneOrMore (deadCell +++ liveCell)

-- Parses a paremetrised block of contiguous same-state cells
paramRow :: Parser [Bool]
paramRow = do
             n <- nat
             b <- liveCell +++ deadCell
             return (replicate n b)

-- Parses a full row, which may have parameterised blocks or not
row :: Parser [Bool]
row = do
        f <- plainRow +++ paramRow
        g <- row
        return (f ++ g)
      +++ return []


-- Parses a line of comments or other non-considered information
infoLine :: Parser ()
infoLine = char '#' >-> ignore ()

---- function for testing
inputR f = do
             file <- readFile f
             case parse ((infoLine >-| offset) >-> oneOrMore inputBlock) file of
                  Just(p,s)  -> print p
                  _          -> print "ERROR"


-- Data type representing a GOL input specification block
data MapBlock = B { topLeft :: Pair, rows :: [[Bool]] }
  deriving (Eq, Show)

-- Parses a world map block
inputBlock :: Parser MapBlock
inputBlock = do
    p <- offset
    m <- oneOrMore (row <-< char '\n')
    let cols = maximum (map length m)
        sqr  = map (\r -> r ++ replicate (cols - length r) False) m
    return (B p sqr)

funcTuple :: ((a -> b), (c -> d)) -> (a, c) -> (b, d)
funcTuple (f, g) (x,y) = (f x, g y)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple h (x, y) = (h x, h y)

-- Updates a given position in a list to a new value
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (i,e) = f e xs i 0
    where
        f _ [] _ _ = []
        f e' (x':xs') n m | n == m = e' : xs'
                          | otherwise = x' : f e' xs' n (m+1)

getSpan :: [MapBlock] -> (Pair, Pair)
getSpan bs = (min_max xIn bs, min_max yIn bs)
    where xIn b = (fst (topLeft b), fst (topLeft b) + length (head (rows b)))
          yIn b = (snd (topLeft b), snd (topLeft b) + length (rows b))

min_max :: (Ord a, Ord b) => (c -> (a, b)) -> [c] -> (a, b)
min_max f bs = funcTuple (minimum, maximum) (unzip (map f bs))

buildW :: (Pair, Pair) -> Int -> [MapBlock] -> World Bool
buildW xsys k bs = undefined
    where dims = mapTuple (*2) (buildDims k xsys)
          base = emptyWorld dims
          mountBlocks = foldl placeBlock base bs

placeBlock :: World Bool -> MapBlock -> World Bool
placeBlock (World d cs) (B (x,y) m) = World d (snd ret)
    where --(x,y) = funcTuple (mapTuple ((-) . (div 2)) d) p   -- equivalent to p - d
          ret = foldl (\(n, w) r -> (n+1, w !!= (n, snd (chunkIn (w !! n) r)))) (y, cs) m
          chunkIn b ts = foldl (\(i, us) v -> (i+1, us !!= (i, v))) (x, b) ts

tim b ts = foldl (\(i, us) v -> (i+1, us !!= (i, v))) (2, b) ts
test = foldl (\(n,l) e -> (n+1, l !!= (n,e))) (1, [1..4]) [1..3]

buildDims :: Int -> (Pair, Pair) -> Pair
buildDims k (αp, ßp) = mapTuple (*k) (maxExt αp, maxExt ßp)
    where maxExt (α, ß) = max (abs α) (abs ß)


-- xs: fst dim
-- ys: snd dim
wordy :: [MapBlock] -> World Bool
--wordy bs = World (xs,ys) rows
wordy = undefined


worldify :: [[Bool]] -> Int -> Int -> [[Bool]]
worldify []     xs ys = replicate ys (replicate xs False)
worldify (r:rs) xs ys = (r ++ replicate (xs - length r) False) : worldify rs xs ys

-- Reads a world map from a specified file
readLife :: FilePath -> IO (World Bool)
readLife f = do
    file <- readFile f
    case parse ((infoLine >-| offset) >-> inputBlock) file of
         Just(p,s) -> case p of
                           B (x,y) (r:rs) -> do
                                               print (x,y)
                                               print $ getSpan (replicate 4 (B (x,y) (r:rs)))
                                               return $ World (length r -x, length (r:rs))
                                                            (worldify (r:rs)
                                                              (length r - x)
                                                              (length (r:rs)))
                           _              -> error "readLife : wrong format or "
                                                   "unsupported format"
         _         -> error "readLife : wrong format or unsupported format"


-----------------------------------------------------------------------------
dimensions = do char '('
                c <- specString "cells "       >-> nat
                l <- specString " length "     >-> nat
                w <- specString " width "      >-> nat
                g <- specString " generation " >-> nat
                char ')'
                return (c,l,w,g)

patternName = do char '"'
                 n <- oneOrMore $ sat (/= '"')
                 char '"'
                 return n

comment = char '!' >-> do
                         char ' '
                         patternName
                         char ' '
                         dimensions
                       +++ ignore (0,0,0,0)


{- File structure
  * comments: start with "!"
  * world general info: ! "some-string" (cells <nat> length <nat> width <nat> generation <nat>)
  * offset: <nat>k<nat>h@!
  * row: (<nat>)?("."+"0")*
         trailing: dead
  * empty row: "." == all dead

-}
