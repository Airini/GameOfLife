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


--specString :: String -> Parser Char
--specString s | length s == 1 = char (head s)
--             | otherwise     = char (head s) >-> specString (tail s)

specString :: String -> Parser String
specString [] = error "specString: empty string"
specString s | length s == 1  = pmap (:[]) (char (head s))
             | otherwise      = char (head s) <:> specString (tail s)

--aux = pmap (\c -> [c]) (specString "#P")

offset :: Parser Pair
--offset = parse (char '#' >-> char 'P' >-> char ' ')
offset = specString "#P" >-> oneOrMore (sat isSpace) >->
         (nat >*> (\y -> oneOrMore (sat isSpace) >->
                        nat >*> \x -> success (x,y))) <-< char '\n'

offset' :: Parser Pair
offset' = specString "#P" >-> oneOrMore (sat isSpace) >->
          do
            y <- nat
            x <- oneOrMore (sat isSpace) >-> nat <-< char '\n'
            return (x,y)

offset'' :: Parser Pair
offset'' = do
             specString "#P"
             oneOrMore (sat isSpace)
             y <- nat
             oneOrMore (sat isSpace)
             x <- nat
             char '\n'
             return (x,y)

deadCell :: Parser Bool
deadCell = char '.' >-> success False

liveCell :: Parser Bool
liveCell = char '*' >-> success True
--liveCell = (char '*' +++ char '0') >-> success True

plainRow :: Parser [Bool]
plainRow = oneOrMore (deadCell +++ liveCell)

paramRow :: Parser [Bool]
paramRow = do
             n <- nat
             b <- liveCell +++ deadCell
             return (replicate n b)

row :: Parser [Bool]
row = do
        f <- plainRow +++ paramRow
        g <- row
        return (f ++ g)
      +++ return []

ignore :: a -> Parser a
ignore t = zeroOrMore (sat (/= '\n')) >-> char '\n' >-> return t

infoLine :: Parser ()
infoLine = char '#' >-> ignore ()

(>-|) :: Parser a -> Parser b -> Parser [a]
p >-| q = (peak q >-> return []) +++ (p <:> (p >-| q))

{-
description :: String -> (Maybe Pair, [String])
description []     = (Nothing, [])
description (l:ls) = let t = parse infoLine l
                     in case t of 
                             Just ((0,0), s) -> description ls
                             Just (p, "")    -> (Just p, ls)
                             Nothing         -> (Nothing, l:ls)
-}
inputR f = do
             file <- readFile f
             case parse ((infoLine >-| offset) >-> oneOrMore inputBlock) file of
                  Just(p,s)  -> print p
                  _          -> print "ERROR"

readLife :: FilePath -> IO (World Bool)
readLife f = do
    file <- readFile f
    case parse ((infoLine >-| offset) >-> inputBlock) file of
         Just(p,s) -> case p of
                           B (x,y) (r:rs) -> return $ World (length r -x, length (r:rs))
                                                            (worldify (r:rs)
                                                              (length r - x)
                                                              (length (r:rs)))
                           _              -> error "readLife : wrong format or "
                                                   "unsupported format"
         _         -> error "readLife : wrong format or unsupported format"
    where worldify []     xs ys = replicate ys (replicate xs False)
          worldify (r:rs) xs ys = (r ++ replicate (xs - length r) False) :
                                  worldify rs xs ys


data MapBlock = B { topLeft :: Pair, rows :: [[Bool]] }
  deriving (Eq, Show)

inputBlock :: Parser MapBlock
inputBlock = do p <- offset
                m <- oneOrMore (plainRow <-< char '\n')
                return (B p m)

{-
block :: [[String]] -> Maybe inputBlock
block [] = Nothing
block (l:ls) = case parse offset l of
                    Just (p, "") -> Just B p     (
                    _            -> 
-}

--readLife :: LiveCell c => FilePath -> IO (World c)
{-readLife f = do
               file <- readFile f
               let txt = lines file
                   (x,y) = case parse offset (head txt) of
                                Just (p,"") -> p
                                _           -> (0,0)
               print (x,y)
               return $ emptyWorld (10,10)
-}

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
