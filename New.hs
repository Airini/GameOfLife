module New where

import GOL
import ReadGOL
import World

test :: FilePath -> IO ()
test f = do
  t <- readLife f 2
  let q = fillCells t :: World Int
      h = tick q
  print (t, q, h, if h==q then "oops" else "mi")
