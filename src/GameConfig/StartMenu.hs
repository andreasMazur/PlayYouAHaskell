
module GameConfig.StartMenu (
    userInterface
) where

import System.Console.ANSI (setTitle,  hideCursor ) 

import GameEngine.Playground (createPlaygroundWithPins,  Playground )
import GameEngine.StandardCalls ( changePg_noP )
import GameEngine.GUI ( GUI(..) )
import GameConfig.UserInterface (debugWindow_P, 
    controlsMenu_P,  
    rootPin,
    multiUseBox_P,
    interBox_P,
    textBox_P
 )
import GameEngine.StandardCalls (displayPinCalls)

import GameEngine.Call (Call, identityCall)

-- | Initial gui-value for the game
userInterface:: IO GUI
userInterface 
    = do rP <- rootPin
         playground <- startMenu_Pg
         pictures <- sequence [ multiUseBox_P,
                                interBox_P,
                                textBox_P,
                                controlsMenu_P,
                                debugWindow_P ]
         return $ GUI {
             root = rP,
             playground = playground,
             windows = pictures
         }

startMenu_Pg:: IO Playground
startMenu_Pg 
    = do hideCursor
         setTitle "Play You a Haskell"
         createPlaygroundWithPins startMenu 
                                  "Start menu" 
                                  [] 
                                  [displayPinCalls [loadIntro]]

loadIntro:: Call
loadIntro = identityCall

startMenu:: String
startMenu = "                                                                                \
            \ Willkommen in 'Play You a Haskell'!                                            \
            \                                                                                \
            \ Starte ein neues Spiel oder lade einen alten Spielstand, indem                 \
            \ du im Interaktionsmenü die entsprechende Interaktion auswählst.                \
            \                                                                                \
            \                                                                                \
            \ Viel Spaß!                                                                     \
            \                                                                                \
            \                                                                                \
            \                                                                                \
            \                                                                                \
            \                                                                                \
            \                                                                                \
            \                                                                                \
            \                                                                                \
            \                                                                                \
            \                                                                                \
            \                                                                                \
            \                                                                                "
