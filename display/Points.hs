module Points where

import Graphics.Rendering.OpenGL
import World

points :: World -> [(GLfloat,GLfloat,GLfloat)]
points w = map (\(x,y,v) -> (x,y,0) ) lives
    where
        c = concatMap (\(y,r) -> map (\(x,v) -> (x,y,v)) (row r) ) rows
        rows = zip [0..] (cells w)
        row  = zip [0..]
        lives = filter (\(_,_,b) -> b == True) c
        d = dim w
