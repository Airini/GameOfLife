module Main where

import GOL
import World
import Graphics.UI.GLUT
import Data.IORef
import Bindings
import Display

main :: IO ()
main = do
         let initW = blinker
         runGame initW

runGame :: (LiveCell a) => World a -> IO ()
runGame w = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _window <- createWindow "Game of Life"
  reshapeCallback $= Just reshape
  world <- newIORef w
  zoom <- newIORef 0.5
  delta <- newIORef 0.1
  pos <- newIORef (0, 0)
  addTimerCallback timerMs (redisplay)
  keyboardMouseCallback $= Just (keyboardMouse zoom delta pos)
  idleCallback $= Just (idle world delta)
  displayCallback $= display world zoom pos
  mainLoop
{-runGame w | w == nextW  = printWorld w-}
          {-| otherwise   = do-}
                            {-printWorld w-}
                            {-runGame nextW-}
    where nextW = tick w

redisplay :: TimerCallback
redisplay = do
    addTimerCallback timerMs (redisplay)

timerMs = 500
