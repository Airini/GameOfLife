module ReadGOL where

import World
import Parsing
import Data.Char

-----------------------------------------------------------------------------
{-= From our module _Sudoku_ (lab 3) =-}

-- Updates a given position in a list to a new value
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (i,e) = f e xs i 0
    where
        f _ [] _ _ = []
        f e' (x':xs') n m | n == m = e' : xs'
                          | otherwise = x' : f e' xs' n (m+1)


-----------------------------------------------------------------------------
{-= Generic non-parse functions =-}

-- A 2-tuple of functions acting on a pair of elements, element-wise
funcTuple :: (a -> b, c -> d) -> (a, c) -> (b, d)
funcTuple (f, g) (x,y) = (f x, g y)

-- Maps a function on each element of a pair
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple h (x, y) = (h x, h y)

-- Minimum and maximum of first components and second components respectively
-- of pairs produced by a function applied to each item in a list
min_max :: (Ord a, Ord b) => (c -> (a, b)) -> [c] -> (a, b)
min_max f bs = funcTuple (minimum, maximum) (unzip (map f bs))


-----------------------------------------------------------------------------
{-= Adapted from _ReadExprMonadic_ =-}

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
{-= Generic parsers =-}

-- Parses using a first parser until an occurrence parsable by a second
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
ignore t = sat (const True) >-| char '\n' >-> char '\n' >-> return t


-----------------------------------------------------------------------------
{-= GOL-file specific parsers (format: Life 1.05 with some variations) =-}

-- Parses a line of comments or other non-considered information
infoLine :: Parser ()
infoLine = char '#' >-> ignore ()

-- Parses an offset (in the world map) pair with respect to the origin (0,0)
offset :: Parser Pair
offset = do
             specString "#P"
             oneOrMore (sat isSpace)
             x <- integer
             oneOrMore (sat isSpace)
             y <- integer
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


-----------------------------------------------------------------------------
{-= Type and parser for text input life map blocks (~ Life 1.05 format) =-}

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


-----------------------------------------------------------------------------
{-= Conversion functions from a set of map blocks to a world map =-}

-- Generates a World map from a list of map blocks; the second parameter is
-- a scale factor for the world's size with respect to the blocks' span
worldify :: [MapBlock] -> Int -> World Bool
worldify ms k = buildW (getSpan ms) k ms

-- Yields the span (across each axis) of all defined cells in a list of map
-- blocks. The return is two _Pair_s: first for x-axis, second for y-axis
-- (in a Cartesian coordinate system). Each pair consists of:
--   (lower limit, upper limit) on the corresponding axis
getSpan :: [MapBlock] -> (Pair, Pair)
getSpan bs = (min_max xIn bs, min_max yIn bs)
    where xIn b = (fst (topLeft b), fst (topLeft b) + length (head (rows b)))
          yIn b = (snd (topLeft b), snd (topLeft b) + length (rows b))

-- Builds a world from a set of map blocks, given a scaling factor and the
-- range in which specified cells exist across each dimension
buildW :: (Pair, Pair) -> Int -> [MapBlock] -> World Bool
buildW xsys k = foldl placeBlock base
    where dims = mapTuple (*2) (buildDims k xsys)
          base = emptyWorld dims

-- Places a map block in a given world
placeBlock :: World Bool -> MapBlock -> World Bool
placeBlock (World d cs) (B p m) = World d (snd blockIn)
    where (x,y) = funcTuple (mapTuple ((+) . (`div` 2)) d) p
                  -- equivalent to d/2 + p
          blockIn = foldl (\(n, w) r ->
                            (n+1, w !!= (n, snd (rowChunkIn (w !! n) r))))
                          (y, cs)
                          m
          rowChunkIn b = foldl (\(i, us) v -> (i+1, us !!= (i, v))) (x, b)

-- Given a scale factor and ranges for each dimension, calculates the
-- dimension of the whole world (considering (0,0) as the centre point)
buildDims :: Int -> (Pair, Pair) -> Pair
buildDims k (αp, ßp) = mapTuple (*k) (maxExt αp, maxExt ßp)
    where maxExt (α, ß) = max (abs α) (abs ß)


-----------------------------------------------------------------------------
{-= World map file reading =-}

-- Reads a world map from a specified file and creates a World with booleans
-- as cells and given a scale factor to determine the size of the whole world
-- with respect to the area specified in the file.
-- * True cells  => alive
-- * False cells => dead
readLife :: FilePath -> Int -> IO (World Bool)
readLife f k = do
    file <- readFile f
    case parse ((infoLine >-| offset) >->
                 oneOrMore inputBlock <-< zeroOrMore (sat isSpace)) file of
         Just(p,"") -> return $ worldify p k
         _          -> error "readLife: ill-formed input life file"

---- function for testing
inputR f = do
    file <- readFile f
    case parse ((infoLine >-| offset) >-> oneOrMore inputBlock) file of
         Just(p,s) -> do
                        print (p,s)
                        print $ buildDims 2 (getSpan p)
                        let a = placeBlock (buildW (getSpan p) 2 p) (head p)
                        print (dim a)
                        print a
                        --print $ parse inputBlock s
         _          -> print "ERROR"

