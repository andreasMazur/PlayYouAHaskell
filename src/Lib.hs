module Lib
    ( startGame
    ) where

import Control.Monad.State ( StateT(runStateT) )

import GameEngine.GameLoop ( initGame )
import GameConfig.StartMenu ( userInterface )


startGame :: IO ()
startGame = do gui <- userInterface
               runStateT initGame gui
               return ()
