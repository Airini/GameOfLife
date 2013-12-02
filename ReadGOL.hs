module ReadGOL where

import GOL
import Parsing

readLife :: FilePath -> IO World
readLife f = return $ emptyWorld (100,100)

specString :: String -> Parser String
specString s | 

--offset :: String -> String
offset = parse (char '#' >-> char 'P' >-> char ' ')

