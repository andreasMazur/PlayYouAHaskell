{-# LANGUAGE RankNTypes #-}

module GameEngine.Textbox where

import GameEngine.Pinboard (background, exchangeBackground, UpdatePinboard)
import GameEngine.Pin (Pin)
import GameConfig.UserInterface (textBoxDim)
import GameEngine.PinUtilities (forcePrintPin, createPin')
import GameEngine.PinboardUtilities (clearPinboard, removePins, printPinboard, addPins)
import Control.Monad.State (StateT(runStateT))
import GameEngine.UserInput (readUserInput)
import GameEngine.Call (setNested, executeCalls, Call)
import System.Console.ANSI (hideCursor, showCursor, setCursorPosition)
import GameEngine.Terminal (resetCursor)

textPinDim:: (Int, Int)
textPinDim = (fst textBoxDim-2, snd textBoxDim)

-- | Displays a given text in the textbox of the game. Then, it waits for a
--   userinput, which is used to determine which call shall be execeuted.
displayQuestion:: String -> (String -> Call) -> UpdatePinboard
displayQuestion text f pb
    = do -- create the text-pin
         textPin <- createTextPin text
         -- add the text-pin to the textbox
         newPb <- addPins [textPin] pb
         -- display the text-pin
         (_, newPb2) <- runStateT (printPinboard (0, 0)) newPb
         -- Read the users answer on the question
         setCursorPosition 28 2
         putStr "Antwort: "
         showCursor
         userInput <- getLine
         hideCursor
         resetCursor
         -- Execute the call based on the users input
         newPb3 <- executeCalls [setNested True $ f userInput] newPb2
         -- remove the text-pin from the pinboard
         newPb4 <- removePins [0] newPb3
         return $ exchangeBackground
              (forcePrintPin.background $ newPb4) newPb4

-- | Displays a given text in the textbox of the game
displayText:: String -> UpdatePinboard
displayText text pb = do -- create the text-pin
                         textPin <- createTextPin text
                         -- add the text-pin to the textbox
                         newPb <- addPins [textPin] pb
                         -- display the text-pin
                         runStateT (printPinboard (0, 0)) newPb
                         -- The next user-input lets the text disappear
                         readUserInput
                         -- remove the text-pin from the pinboard
                         newPb2 <- removePins [0] newPb
                         -- remove "the appearence" of the text in the terminal
                         clearPinboard (newPb, newPb2)
                         return newPb2

-- | Displays a given text in the textbox of the game
displayText':: String -> UpdatePinboard
displayText' text pb = do -- create the text-pin
                          textPin <- createTextPin text
                          -- add the text-pin to the textbox
                          newPb <- addPins [textPin] pb
                          -- display the text-pin
                          runStateT (printPinboard (0, 0)) newPb
                          -- remove the text-pin from the pinboard
                          removePins [0] newPb

createTextPin:: String -> IO Pin
createTextPin text = createPin' (fillMissing.fitText $ text) textPinDim (0, 0) 0 []

-- | Adds spaces to a given text, such that the text contains enough characters
--   for the text-pin
fillMissing:: String -> String
fillMissing text
    = text ++ [' ' | _ <- [1..uncurry (*) textPinDim - length text]]

fitText:: String -> String
fitText text = createRows preProcessedText
    where
        preProcessedText:: [String]
        preProcessedText = map (++ " ").words $ text

createRows:: [String] -> String
createRows [] = []
createRows words = row ++ createRows residualWords
    where
        (row, residualWords) = createRow words ("", 0)

createRow:: [String] -> (String, Int) -> (String, [String])
createRow [] (row, _) = (row, [])
createRow (word : ws) (prevWords, takenSpace)
    | takenSpace + length word < snd textBoxDim
    = createRow ws (prevWords ++ word, takenSpace + length word)
    | otherwise
    = (prevWords ++ [' ' | _ <- [1..snd textBoxDim - takenSpace]], word : ws)

