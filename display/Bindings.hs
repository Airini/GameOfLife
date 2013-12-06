module Bindings (display, reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Data.IORef
import Display

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

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
