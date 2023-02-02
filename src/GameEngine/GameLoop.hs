
module GameEngine.GameLoop where

import Control.Monad.State ( 
    MonadIO(liftIO), 
    MonadState(get), 
    StateT 
 )
import System.Timeout (timeout)
import System.Console.ANSI (showCursor, setCursorPosition, clearScreen)

import GameEngine.GUI (
    GUI(..),
    alterGUI,
    alterGUIRoots,
    printGUI,
    clearGUI,
 )
import GameEngine.CallHandler ( callHandler )
import GameEngine.UserInput ( readUserInput )
import GameEngine.RegisterHandler (itemParser, callParser)

import GameConfig.KeyBindings ( Update(..), findBinding )

import Physics.PhysicsHandler ( applyPhysics )

{-|
    The update rate sets the game speed. Smaller update rates allow a more frequent
    game update, which causes the game to run faster. Higher update rates let the
    game run slower.
-}
update_rate:: Int
update_rate = 50000

-- | Update rate when debugging PYaH
update_rate_debug_mode:: Int
update_rate_debug_mode = 1000000

-- | Starts PYaH
initGame:: StateT GUI IO GUI
initGame = do callHandler
              gui <- get
              drawGame gui
              gameLoop

{-|
    The game-loop reads user inputs as well as controls the order of the 'callHandler'
    and 'physics'-updates.
-}
gameLoop:: StateT GUI IO GUI
gameLoop = do gui <- get
              inTime <- liftIO $ timeout update_rate readUserInput
              case inTime of
                  Just c -> do if c == 'p'
                                 then do liftIO clearScreen
                                         liftIO $ setCursorPosition 0 0
                                         liftIO showCursor
                                         liftIO $ print "Bis zum naechsten Mal!" 
                                         liftIO callParser
                                         liftIO itemParser
                                         return gui
                                 else do let updateFunctions = findBinding c
                                         updatePinboards updateFunctions
                                         physics updateFunctions
                                         drawGame gui
                                         gameLoop
                  Nothing -> do physics $ Update id return id
                                drawGame gui
                                gameLoop

-- | Applies changes on roots, pinboards and playgrounds
updatePinboards:: Update -- ^ The changes to apply on roots, pinboards and playground
               -> StateT GUI IO GUI
updatePinboards update = do alterGUIRoots $ updatePin update
                            alterGUI $ updatePinboard update
                            callHandler

-- | Applies changes, which are caused by physics, onto the playground
physics:: Update -- ^ The physics-changes to apply on the playground
       -> StateT GUI IO GUI
physics update = do applyPhysics $ updatePlayground update
                    callHandler

-- | Draws the game by clearing the 'GUI' and subsequently drawing the updates
drawGame:: GUI -- ^ The 'GUI' to draw
        -> StateT GUI IO GUI
drawGame gui = do clearGUI gui
                  printGUI
