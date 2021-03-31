{-# LANGUAGE RankNTypes #-}

module GameConfig.KeyBindings (
    Update(..),
    findBinding
) where

import Data.Array ( Array, (!), array )
import Data.List ( (\\) )

import GameEngine.Pinboard ( UpdatePinboard )
import GameEngine.Pin ( UpdatePin )
import GameEngine.PinboardUtilities ( forcePrintPinboard )
import GameEngine.Playground ( UpdatePlayground )
import GameEngine.PinUtilities (forcePrintPin )
import GameEngine.Menu ( enterMenu )

import GameConfig.PlayerModel ( playerModel_ID )

import Physics.MovementV2 ( moveLeft, moveRight, jumpUp, jumpRight, jumpLeft )
import GameConfig.UserInterface (multiUseBoxID, interBoxID)

type UserInput = Char
data Update = Update { updatePin :: UpdatePin,  -- applied on pinboard-roots
                       updatePinboard :: UpdatePinboard, -- applied on pinboard
                       updatePlayground :: UpdatePlayground }  -- applied only on playground-pins

-- | Constants for the binding-register
inputRange :: [Char]
inputRange = [firstIndex..lastIndex]

firstIndex :: Char
firstIndex = ' '

lastIndex :: Char
lastIndex = 'z'

-- | Function to map the user-input onto its correct update
-- | 'userInput' : The user-input.
findBinding:: UserInput -> Update
findBinding userInput
    | userInput `elem` inputRange = bindingRegister ! userInput
    | otherwise                   = Update id return id

-- | Register used to bind keys onto updates.
bindingRegister:: Array UserInput Update
bindingRegister = array (firstIndex, lastIndex) finalBindings
    where
        bindings:: [(UserInput, Update)]
        bindings = [ ('w', Update id return (jumpUp playerModel_ID)),
                     ('e', Update id return (jumpRight playerModel_ID)),
                     ('q', Update id return (jumpLeft playerModel_ID)),
                     ('a', Update id return (moveLeft playerModel_ID)),
                     ('d', Update id return (moveRight playerModel_ID)),
                     ('m', Update id (enterMenu interBoxID) id),
                     ('i', Update id (enterMenu multiUseBoxID) id),
                     ('r', Update forcePrintPin forcePrintPinboard id) ]
        finalBindings:: [(UserInput, Update)]
        finalBindings = [(char, Update id return id)
                        | char <- freeChars] ++ bindings
        freeChars:: [UserInput]
        freeChars = inputRange \\ map fst bindings
