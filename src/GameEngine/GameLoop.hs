
module GameEngine.GameLoop where

import Control.Monad.State ( 
    MonadIO(liftIO), 
    MonadState(get), 
    StateT 
 )
import System.Timeout (timeout)

import GameEngine.GUI (
    GUI(..),
    alterGUI,
    alterGUIRoots,
    printGUI,
    clearGUI,
 )
import GameEngine.CallHandler ( callHandler )
import GameEngine.UserInput ( readUserInput )

import GameConfig.KeyBindings ( Update(..), findBinding )

import Physics.PhysicsHandler ( applyPhysics )
import GameEngine.RegisterHandler (itemParser, callParser)
import System.Console.ANSI (showCursor, setCursorPosition, clearScreen)

update_rate:: Int
update_rate = 50000

update_rate_debug_mode:: Int
update_rate_debug_mode = 1000000

initGame:: StateT GUI IO GUI
initGame = do callHandler
              gui <- get
              drawGame gui
              gameLoop

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

updatePinboards:: Update -> StateT GUI IO GUI
updatePinboards update = do alterGUIRoots $ updatePin update
                            alterGUI $ updatePinboard update
                            callHandler

physics:: Update -> StateT GUI IO GUI
physics update = do applyPhysics $ updatePlayground update
                    callHandler

drawGame:: GUI -> StateT GUI IO GUI
drawGame gui = do clearGUI gui
                  printGUI
