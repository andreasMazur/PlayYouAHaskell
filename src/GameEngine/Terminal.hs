
module GameEngine.Terminal where

import System.Console.ANSI ( setCursorPosition )


defaultCursorPosition:: (Int, Int)
defaultCursorPosition = (28, 78)

-- | Resets the cursor to the default position
resetCursor:: IO ()
resetCursor = setCursorPosition (fst defaultCursorPosition) (snd defaultCursorPosition)

