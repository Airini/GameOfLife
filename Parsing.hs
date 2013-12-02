module Parsing
 ( Parser -- exports the type name but not the constructors
  ,parse,
  success,failure,sat,pmap,item,char,digit,
  (+++),(<:>),(>*>),(>->),(<-<),
  oneOrMore,zeroOrMore,chain ) 
{----------------------
Week 5A part I

Same as RefactoredParser.hs but now
Parser added to typeclass Monad
----------------------}
where

import Data.Char
------------------

newtype Parser a = P (String -> Maybe (a,String))

parse :: Parser a -> String -> Maybe(a,String)
-- run a parser on a given string
parse (P f) = f

-------------------
-- Basic Parsers, dependent on internal structure -- 
-- success and fail
failure :: Parser a -- always fails
failure    = P $ \s -> 
             Nothing
 
success :: a -> Parser a 
-- succeeds without looking at the string
success a  = P $ \s -> 
             Just (a,s)

-- Parse any single character
item  = P $ \s ->
        case s of 
             (c:s') -> Just(c,s')
             _      -> Nothing

-- (+++)  parse either using p or else using q
infixr 5 +++
(+++) :: Parser a -> Parser a -> Parser a
p +++ q  = P $ \s ->
           case (parse p s) of
                Just (a,s') -> Just (a,s')
                Nothing     -> parse q s

-- (p >*> f) parse using p to produce a. 
-- Then parse using f a 

infixl 1 >*>

(>*>) :: Parser a -> (a -> Parser b) -> Parser b
p >*> f  = P $ \s -> 
           case parse p s of 
                Just(a,s') -> parse (f a) s'
                Nothing    -> Nothing

-- example: parse any lowercase letter 
-- followed by its uppercase equivalent aA or bB etc.

lowerUpper = item >*> \c -> char (toUpper c) 
-----------------------------------------------
-- Parsers below do not depend on the internal 
-- representation of Parser

-- sat p parse a single character satisfying property p
sat :: (Char -> Bool) -> Parser Char
sat p  = item >*> \c -> if p c then success c else failure 

char c = sat (==c)
digit  = sat isDigit

-- pmap modifies the result of a parser

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = p >*> \a -> success (f a)

(>->) :: Parser a -> Parser b -> Parser b
p >-> q = p >*> \_ -> q

(<-<) :: Parser b -> Parser a -> Parser b
p <-< q = p >*> \a -> q >-> success a

(<:>):: Parser a -> Parser [a] -> Parser [a]
p <:> q = -- p >*> \a -> pmap (a:) q
  do a <- p
     as <- q
     return $ a:as
  
oneOrMore, zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p +++ success []
oneOrMore p  = p <:> zeroOrMore p

chain :: Parser a -> Parser b -> Parser [a]
-- parse a list of as which are separated by bs
chain p q = p <:> zeroOrMore (q >-> p)

-- example: comma separated digits "1,2,3"
diglist = chain digit (char ',') 





instance Monad Parser where
  (>>=)  = (>*>)
  return = success
