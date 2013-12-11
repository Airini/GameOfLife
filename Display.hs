module Display (reshape, keyboardMouse, display) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import World
import GOL

-- Reshapes the window when its size changed
reshape :: ReshapeCallback
reshape size = viewport $= (Position 0 0, size)

-- Binds keyboard actions to zooming and camera positioning
keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse z p key Down _ _ = case key of
  (Char 'q') -> leaveMainLoop
  (Char '+') -> z $~! (* 1.5)
  (Char '-') -> z $~! (/ 1.5)
  (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y-0.1)
  (SpecialKey KeyDown ) -> p $~! \(x,y) -> (x,y+0.1)
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()

-- Draws the world with the zoom level and camera position
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
      color $ Color3      (r c)         (g c)         (b c)
      translate $ Vector3 ((-dimX w)/2) ((-dimY w)/2) (0::GLfloat)
      translate $ Vector3 x             y             (0::GLfloat)
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


-- Generates points for living cells in the world
points :: LiveCell a => World a -> [(GLfloat,GLfloat,a)]
points w = map (\(x,y,v) -> (x,y,v) ) lives
    where
        c = concatMap (\(y,r) -> map (\(x,v) -> (x,y,v)) (row r) ) rows
        rows = zip [0..] (cells w)
        row  = zip [0..]
        lives = filter (\(_,_,b) -> isAlive b) c
        d = dim w

vertex2f :: (GLfloat, GLfloat) -> IO ()
vertex2f (x, y) = vertex $ Vertex2 x y

-- Draws a cube with a given size
cube :: GLfloat -> IO ()
cube w = renderPrimitive QuadStrip $ mapM_ vertex2f
         [(x,y) | x <- [w,-w], y <- [w,-w]]
