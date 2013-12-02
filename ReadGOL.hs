module ReadGOL where

import GOL
import Parsing
import Data.Char

readLife :: FilePath -> IO World
readLife f = return $ emptyWorld (100,100)

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



