module ReadGOL where

import GOL
import Parsing
import Data.Char
import Data.Maybe

readLife :: FilePath -> IO World
readLife f = do
               file <- readFile f
               let txt = lines file
                   (x,y) = case parse offset (head txt) of
                                Just (p,"") -> p
                                _           -> (0,0)
               print (x,y)
               return $ emptyWorld (10,10)


--specString :: String -> Parser Char
--specString s | length s == 1 = char (head s)
--             | otherwise     = char (head s) >-> specString (tail s)

specString :: String -> Parser String
specString [] = error "specString: empty string"
specString s | length s == 1  = pmap (:[]) (char (head s))
             | otherwise      = char (head s) <:> specString (tail s)

--aux = pmap (\c -> [c]) (specString "#P")

-- From ReadExprMonadic
nat :: Parser Int
nat = do ds <- oneOrMore digit
         return (read ds)


offset :: Parser Pair
--offset = parse (char '#' >-> char 'P' >-> char ' ')
offset = specString "#P" >-> oneOrMore (sat isSpace) >->
         nat >*> (\y -> oneOrMore (sat isSpace) >->
                        nat >*> \x -> success (x,y))

offset' :: Parser Pair
offset' = specString "#P" >-> oneOrMore (sat isSpace) >->
          do
            y <- nat
            x <- oneOrMore (sat isSpace) >-> nat
            return (x,y)

offset'' :: Parser Pair
offset'' = do
             specString "#P"
             oneOrMore (sat isSpace)
             y <- nat
             oneOrMore (sat isSpace)
             x <- nat
             return (x,y)

deadCell :: Parser Bool
deadCell = char '.' >-> success False

liveCell :: Parser Bool
liveCell = char '*' >-> success True

row :: Parser [Bool]
row = oneOrMore (deadCell +++ liveCell)



