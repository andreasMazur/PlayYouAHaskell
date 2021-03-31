
module GameEngine.UserInput where

import GameEngine.Terminal ( resetCursor )
import GameEngine.BufferFix ( userInput )

readUserInput:: IO Char
readUserInput = do userInput <- userInput
                   resetCursor
                   return userInput
