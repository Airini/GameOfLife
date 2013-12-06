module Display ( display) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Cube
import Points
import World
import GOL

display :: LiveCell a => IORef (World a) -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display world zoom pos = do
  clear [ColorBuffer, DepthBuffer]
  clear [ColorBuffer]
  loadIdentity
  w <- get world
  (x',y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    z' <- get zoom
    scale z' z' z'
    forM_ (points w) $ \(x,y,c) -> preservingMatrix $ do
      color $ Color3      (r c)     (g c)     (b c)
      translate $ Vector3 ((-dimX w)/2) ((-dimY w)/2) (0::GLfloat)
      translate $ Vector3 x         y         (0::GLfloat)
      cube 0.4
  swapBuffers
  postRedisplay Nothing
  where dimX = fst' . dim
        dimY = snd' . dim
        fst' = fromIntegral . fst
        snd' = fromIntegral . snd
        r = (\(x,_,_) -> float2GLfloat x) . getColour
        g = (\(_,x,_) -> float2GLfloat x) . getColour
        b = (\(_,_,x) -> float2GLfloat x) . getColour
        float2GLfloat x = realToFrac x ::GLfloat
