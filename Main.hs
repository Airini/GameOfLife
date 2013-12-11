module Main where

import GOL
import World
import Graphics.UI.GLUT
import Data.IORef
{-import Bindings-}
import Display
import ReadGOL
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        let file | null args = error "\nDear Sir!\nGive me a GOL file!"
                 | otherwise = head args
        simpleWorld <- readLife file 1

        select simpleWorld args
    where select w as | length as == 2 &&
                        as !! 1 == "aging" = runGame (fillCells w ::World Int)
                      | otherwise = runGame w

runGame :: (LiveCell a) => World a -> IO ()
runGame w = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "Game of Life"
    reshapeCallback $= Just reshape
    world <- newIORef w
    zoom <- newIORef 0.25
    pos <- newIORef (0, 0)
    addTimerCallback timerMs (redisplay world)
    keyboardMouseCallback $= Just (keyboardMouse zoom pos)
    displayCallback $= display world zoom pos
    mainLoop

redisplay :: LiveCell a => IORef (World a) -> TimerCallback
redisplay w = do
    w $~! tick
    addTimerCallback timerMs (redisplay w)

timerMs = 100
