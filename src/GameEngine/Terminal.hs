
module GameEngine.Terminal where

import System.Console.ANSI ( setCursorPosition )


defaultCursorPosition:: (Int, Int)
defaultCursorPosition = (28, 78)

resetCursor:: IO ()
resetCursor 
    = setCursorPosition (fst defaultCursorPosition) (snd defaultCursorPosition)

