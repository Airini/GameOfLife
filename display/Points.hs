module Points where

import Graphics.Rendering.OpenGL
import World

points :: LiveCell a => World a -> [(GLfloat,GLfloat,a)]
points w = map (\(x,y,v) -> (x,y,v) ) lives
    where
        c = concatMap (\(y,r) -> map (\(x,v) -> (x,y,v)) (row r) ) rows
        rows = zip [0..] (cells w)
        row  = zip [0..]
        lives = filter (\(_,_,b) -> isAlive b) c
        d = dim w
