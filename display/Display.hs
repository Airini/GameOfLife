module Display (idle, display) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Cube
import Points
import World

display :: IORef GLfloat -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display zoom angle pos = do
  clear [ColorBuffer, DepthBuffer]
  clear [ColorBuffer]
  loadIdentity
  (x',y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    z' <- get zoom
    scale z' z' z'
    forM_ (points $ w) $ \(x,y,z) -> preservingMatrix $ do
      color $ Color3      r         g         (b::GLfloat)
      translate $ Vector3 (-dimX/2) (-dimY/2) (0::GLfloat)
      translate $ Vector3 x         y         (0::GLfloat)
      cube 0.4
  swapBuffers
  where w = emptyWorld d  -- stub for world to be draw
        d = (5,5)         -- stub for world to be draw
        dimX = fst' $ dim w
        dimY = snd' $ dim w
        fst' = fromIntegral . fst
        snd' = fromIntegral . snd
        r = (\(x,_,_) -> x) getColour
        g = (\(_,x,_) -> x) getColour
        b = (\(_,_,x) -> x) getColour

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing
