{-# LANGUAGE RankNTypes #-}

module GameEngine.Menu (
    enterMenu,
    addNewIBoxCalls,
    updateIBoxCalls
) where

import Control.Monad.State (execState, runStateT )
import Data.List ((\\))

import GameEngine.UserInput ( readUserInput )
import GameEngine.PinUtilities (setPinCallsPersistence, createPin', alterPin', setPosition, setId )
import GameEngine.PinboardUtilities (printPinboardNoLoop, addCalls, alterSpecificPin,
    getPins,
    addPin,
    forcePrintPinboard,
    removePins,
    printPinboard,
    clearPinboard,
    accessPins,
    addPins
 )
import GameEngine.Pinboard ( exchangePins, Pinboard, pins, id_pb,  UpdatePinboard )
import GameEngine.Pin (startIndex, UpdatePin, Pin(..) )
import GameEngine.Call (executeCalls,  Call(..) )
import GameConfig.UserInterface (multiUseBoxID, interBoxID)
import GameEngine.StandardCalls (removeInteractionsPg)

type Identifier = Int
type StartIndex = (Int, Int)


-- ## CONFIGURATION ##

-- | Lowest row for the arrow-pin
arrowLowest:: Int
arrowLowest = 1

-- | Highest row for the arrow-pin (first for menu, second for multi-use box)
arrowHighest:: (Int, Int)
arrowHighest = (6, 10)

-- | Start column for the arrow-pin
startColumn:: Int
startColumn = 4

-- | Arrow 'Pin' ID
arrowID:: Identifier
arrowID = 0

-- | Start position within menu
arrowStartIndex:: StartIndex
arrowStartIndex = (0, 1)

-- | Arrow-'Pin'
arrow:: IO Pin
arrow = createPin' "->" (1, 2) arrowStartIndex arrowID []

-- ## SETTINGS END ##

-- | Computes the start-index of the first new pin, that shall be added to the
--   interaction menu.
-- | 'a' : The interaction menu, in which the start-index shall be computed
computeStartIndex:: Pinboard a => a -- ^ 
                               -> Int
computeStartIndex pb = let startIndices = [fst.startIndex $ p | p <- pins pb]
                       in case startIndices of [] -> 0
                                               _  -> (last startIndices) + 1

{-|
    Displays exactly the calls, which are passed as arguments to this functions.
    Everything else will be removed.
-}
updateIBoxCalls:: [Call] -- ^ The default-calls
               -> [Call] -- ^ The additional calls
               -> UpdatePinboard
updateIBoxCalls defaultCalls calls pb
    = let oldCalls = filter (not.persistence) $ concat [calls_pin p | p <- pins pb]
          calls2Remove = oldCalls \\ calls
      in do newPb <- removeIBoxCalls calls2Remove pb
            addNewIBoxCalls (defaultCalls ++ calls) newPb

-- | Adds ONLY NEW interactions and pin-calls to the interaction menu.
addNewIBoxCalls:: [Call] -- ^ The calls that shall be added
               -> UpdatePinboard
addNewIBoxCalls calls pb
    = let oldCalls = concat [calls_pin p | p <- pins pb]
          calls2Add = calls \\ oldCalls
      in addIBoxCalls calls2Add pb

-- | Adds ALL listed calls to the interaction menu.
addIBoxCalls:: [Call] -- ^ The calls that shall be added
            -> UpdatePinboard
addIBoxCalls calls pb
    = do let startIndex = computeStartIndex pb
         calls <- sequence [ createPin' (description call)
                                        (1, length $ description call)
                                        (id, startColumn)
                                        (id + 1)
                                        [call]
                            | (id, call) <- zip [startIndex..] calls ]
         addPins calls pb

-- | Removes the listed calls.
removeIBoxCalls:: [Call] -- ^ The calls that shall be removed
               -> UpdatePinboard
removeIBoxCalls calls pb
    = let leftPins = [ p | p <- pins pb, checkCalls $ calls_pin p ]
          newPins = [ execState (alterPin' (combinatorFunc (id, startColumn) (id + 1))) p
                    | (id, p) <- zip [0..] leftPins ]
      in do newPb <- removePins (map id_pin $ pins pb \\ leftPins) pb
            return $ exchangePins newPins newPb
    where
        checkCalls:: [Call] -> Bool
        checkCalls cs
            = case cs of []        -> False
                         [element] ->  not $ element `elem` calls
        combinatorFunc:: StartIndex -> Identifier -> UpdatePin
        combinatorFunc newPos newId pin = setPosition newPos $ setId newId pin

-- | Function that moves the arrow-pin one row up
moveArrowUp:: UpdatePin
moveArrowUp pin
    = if id_pin pin == arrowID
        then if arrowLowest <= fst (startIndex pin)
               then pin {startIndex = let (y, x) = startIndex pin
                                      in (y - 1, x)}
               else pin
        else pin

-- | Function that moves the arrow-pin one row down
moveArrowDown:: Identifier  -- ^ menu ID (i.e. pinboard ID)
             -> UpdatePin
moveArrowDown id pin
    = if id_pin pin == arrowID
        then if fst (startIndex pin) <= decision id
               then pin {startIndex = let (y, x) = startIndex pin
                                      in (y + 1, x)}
               else pin
        else pin

-- | Selects highest arrow row depending on selected menu
decision:: Identifier -> Int
decision id
    | id == interBoxID    = fst arrowHighest
    | id == multiUseBoxID = snd arrowHighest
    | otherwise           = error "No know menu-ID."

-- | Interaction menu loop
interactionMenu:: UpdatePinboard
interactionMenu pb
    = do userInput <- readUserInput
         case userInput of
             'w'  -> do newPb <- accessPins moveArrowUp pb
                        newPb2 <- printPinboardNoLoop newPb pb
                        interactionMenu newPb2
             's'  -> do newPb <- accessPins (moveArrowDown $ id_pb pb) pb
                        newPb2 <- printPinboardNoLoop newPb pb
                        interactionMenu newPb2
             'l'  -> do newPb <- removePins [arrowID] pb
                        clearPinboard (pb, newPb)
                        forcePrintPinboard newPb
             '\n' -> do -- Get line of arrow
                        let arrowRow = fst $ startIndex
                                           $ head
                                           $ getPins arrowID pb
                        -- Get pin with pin call
                            func = getPins (arrowRow + 1) pb
                        case func of
                            []        -> interactionMenu pb
                            [pinCall] -> do -- Execute stored pin call
                                            newPb <- executeCalls (calls_pin pinCall) pb
                                            -- Remove the persistent-attribute if call is executed
                                            newPb2 <- alterSpecificPin (setPinCallsPersistence [False]) (id_pin pinCall) newPb
                                            -- Remove arrow from pinboard
                                            newPb3 <- removePins [arrowID] newPb2
                                            clearPinboard (pb, newPb3)
                                            return $ addCalls [removeInteractionsPg $ calls_pin pinCall] newPb3
             _ -> interactionMenu pb

{-|
    Function that transitions the user-activity from the playground-pinboard 
    to the interaction menu-pinboard.
-}
enterMenu:: Identifier -- ^ menu ID (i.e. pinboard ID)
         -> UpdatePinboard
enterMenu id pb
    = if id_pb pb == id
        then do a <- arrow
                -- add pin to the interaction menu
                pb_withArrow <- addPin a pb
                -- display new pin
                pb' <- runStateT (printPinboard (0, 0)) pb_withArrow
                -- 'loop' until decision
                interactionMenu $ snd pb'
        else return pb
