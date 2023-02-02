{-# LANGUAGE RankNTypes #-}

module GameEngine.StandardCalls where

import GameEngine.Pinboard (UpdatePinboard )
import GameEngine.Call (setNested, combineCalls, executeCalls, createCall,  Call(..) )
import {-# SOURCE #-} GameEngine.Menu ( addNewIBoxCalls, updateIBoxCalls )

import GameConfig.UserInterface (textBoxID, interBoxID, playgroundID)
import GameEngine.Textbox (displayText', displayQuestion, displayText)
import GameEngine.PinboardUtilities (alterSpecificPin, addPins, printPinboardNoLoop, alterPinboard, removePins, accessPins, getPins, removeAllPins)
import {-# SOURCE #-} GameEngine.Playground (Playground, UpdatePlayground, applyOnObject)
import GameConfig.PlayerModel (playerModel_ID)
import GameEngine.Object (createObject, Object, removeItemsObj, addItemsObj, setPositionObj)
import {-# SOURCE #-} GameEngine.Item (display_item, removeVisibility, description_item,  Item )
import GameEngine.Pin ( UpdatePin, startIndex,  Pin(inventory) )
import GameEngine.PinUtilities
    (changeAppearence, addItems,  alterPin', removeCalls, setStatus, removeItems )
import {-# SOURCE #-} GameEngine.Playground (addObject)
import Control.Monad.State (execState, runStateT, runState)
import {-# SOURCE #-} GameEngine.TransitionRegister (identityPg, transitionRegister)
import Control.Applicative (Applicative(liftA2))
import GHC.IO (unsafePerformIO)
import GameEngine.Save (saveGame)
import GameEngine.Picture (Picture)

import {-# SOURCE #-} GameEngine.SaveGameLoad ( loadGame )
import Physics.MovementV2 (addVelocity)
import GameEngine.Grid (createGrid)
import Control.Concurrent (threadDelay)

type Offset = (Int, Int)
type Identifier = Int
type Dimension = (Int, Int)
type StartIndex = (Int, Int)

-- ## CALLS ##

-- | Default calls for the interaction menu (are always displayed)
defaultCalls:: [Call]
defaultCalls = [saveGame, loadGame]

-- | A 'Call' that displays the given and default 'Call's in the interaction box (and removes all other 'Call's)
updateCalls:: [Call] -- ^ The 'Call's to display
           -> Call
updateCalls calls = createCall (-2) "display pins" interBoxID (updateIBoxCalls defaultCalls calls) id False "updateCalls"

-- | A 'Call' that adds new 'Call's into the interaction box
displayPinCalls:: [Call] -- ^ The 'Call's to add
               -> Call
displayPinCalls calls = createCall (-3) "display pins" interBoxID (addNewIBoxCalls calls) id False "displayPinCalls"

-- | A 'Call' that removes all the 'Pin's in the interaction box
resetInteractions:: Call
resetInteractions = createCall (-4) "remove pins" interBoxID removeAllPins id False "resetInteractions"

-- | A 'Call' that removes given 'Call's from the 'Playground'
removeInteractionsPg:: [Call] -- ^ The 'Call's to remove
                    -> Call
removeInteractionsPg inters = createCall (-5) "remove interactions" playgroundID theTask id False "removeInteractionsPg"
  where
    theTask:: UpdatePinboard
    theTask pb = accessPins (removeCalls inters) pb


-- ## CHANGE PLAYGROUND ##

-- # WITHOUT PLAYER # 

changePg_noP:: String -- ^ The description of the 'Playground'-change
            -> IO Playground -- ^ The new 'Playground'
            -> String -- ^ The save-name of this 'Call'
            -> Call
changePg_noP title newPg name = Call {
    id_c = -7,
    description = title,
    initiator = -1,
    receiver = -1,
    task = return,
    interaction = id,
    persistence = True,
    disposable = False,
    nestedCall = False,
    newPlayground = Just newPg,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
 }

-- # WITH PLAYER # 

-- | Change playground without changing the players position
changePg:: String -- ^ The description of the 'Playground'-change
        -> IO Playground -- ^ The new 'Playground'
        -> String -- ^ The save-name of this 'Call'
        -> Call
changePg title newPg = createCall (-8) title playgroundID (changePgHelp title newPg) id False

changePgHelp:: String -> IO Playground -> UpdatePinboard
changePgHelp title newPg pb
  = do let player = head $ getPins playerModel_ID pb
       executeCalls [changePg' title (createObject $ setStatus True player) newPg] pb

-- | Change playground and change the players position
changePgNewPos:: Offset -- ^ The new start-index of the player model
              -> String -- ^ The description of the 'Playground'-change
              -> IO Playground -- ^ The new 'Playground'
              -> String -- ^ The save-name of this 'Call'
              -> Call
changePgNewPos newPos title newPg = createCall (-8)
                 title
                 playgroundID
                 (changePgHelp' newPos title newPg)
                 id
                 False

changePgHelp':: Offset -> String -> IO Playground -> UpdatePinboard
changePgHelp' newPos title newPg pb
    = do let player = alteredPlayer newPos.head $ getPins playerModel_ID pb
         executeCalls [changePg' title (createObject $ setStatus True player) newPg] pb

-- | Move the player model
alteredPlayer:: Offset -- ^ The new start-index of the player model
              -> UpdatePin
alteredPlayer newPos player = snd $ runState (alterPin' $ \p -> p { startIndex = newPos } ) player

-- | Change the 'Playground' while simultaneously adding the player model
changePg':: String -- ^ The description of the 'Playground'-change
         -> Object -- ^ The player model as an 'Object'
         -> IO Playground -- ^ The new 'Playground'
         -> Call
changePg' title player newPb = Call {
    id_c = -9,
    description = title ++ "_execute",
    initiator = -1,
    receiver = -1,
    task = return,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = True,
    newPlayground = Just $ addObject player newPb,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = "intermediate call",
    tripwire = False
 }

-- ## CHANGE PLAYGROUND WITH PLAYER - END ##

-- ## HOT SWAP - PLAYGROUND ##

-- # SWAP OBJECTS TEMPLATES #

-- | Swap playground, keep the players position
swapObj:: StartIndex -- ^ Player position
       -> Call -- ^ The swap-'Call'
       -> (String, Dimension, StartIndex, [Call], Bool, Bool, [Item])
swapObj sI swap = (" ", (1, 1), sI, [swap], True, True, [])

-- | Swap playground, keep the players position (with identifier)
swapObj':: StartIndex -- ^ Player position
        -> Call -- ^ The swap-'Call'
        -> Identifier -- ^ Identifier of the player object
        -> (String, Dimension, StartIndex, [Call], Bool, Bool, [Item], Identifier)
swapObj' sI swap id = (" ", (1, 1), sI, [swap], True, True, [], id)


-- # HOT-SWAP CALLS #

-- | Swap playground API
swapPg:: UpdatePinboard -- ^ Function that contains the swap-task as a nested 'Call'. See 'swapPgHelp' or 
                        --   'swapPgHelpNewPos'.
      -> String -- ^ The save-name of this 'Call'
      -> Call
swapPg f name = Call {
    id_c = -12,
    description = "playground hot swap",
    initiator = -1,
    receiver = playgroundID,
    task = f,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = True,
    callName = name,
    tripwire = False
}

-- | Swap playground task: Executes nested 'Call' with the players current position
swapPgHelp:: IO Playground -- ^ The new 'Playground'
          -> UpdatePinboard
swapPgHelp newPg pb
  = do let player = head $ getPins playerModel_ID pb
       executeCalls [swapPg' (createObject $ newCoord player) newPg] pb
  where
    newCoord:: UpdatePin
    newCoord player
      | (snd.startIndex $ player) /= 3
      = player { startIndex = (fst.startIndex $ player, 5) }
      | otherwise
      = player { startIndex = (fst.startIndex $ player, 72) }

-- | Swap playground task: Executes nested 'Call' with the players new position
swapPgHelpNewPos:: Offset -- ^ New position of the player model
                -> IO Playground -- ^ The new 'Playground'
                -> UpdatePinboard
swapPgHelpNewPos newCoord newPg pb
  = do let player = head $ getPins playerModel_ID pb
       executeCalls [swapPg' (createObject $ alteredPlayer newCoord player) newPg] pb

-- | The playground-'hot-swap'-call (immediatly executes swap of the playground)
swapPg':: Object -- ^ The player model as an 'Object'
       -> IO Playground -- ^ The new 'Playground'
       -> Call
swapPg' player newPg = Call {
    id_c = -13,
    description = "playground hot swap_execute",
    initiator = -1,
    receiver = -1,
    task = return,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = True,
    newPlayground = Just $ addObject player newPg,
    newPicture = Nothing,
    executeImmediatly = True,
    callName = "nested Call",
    tripwire = False
}

-- ## HOT SWAP - PLAYGROUND - END ##

-- ## CHANGE PICTURE ##

-- | Changes a 'Picture'
changePic:: String -- ^ The description for the 'Picture'-change 
         -> IO Picture -- ^ The new 'Picture'
         -> Identifier -- ^ The ID of the target 'Picture' within the GUI
         -> String -- ^ The save-name of this 'Call'
         -> Call
changePic title newPic targetPb name = Call {
    id_c = -7,
    description = title,
    initiator = -1,
    receiver = targetPb,
    task = return,
    interaction = id,
    persistence = True,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Just newPic,
    executeImmediatly = False,
    callName = name,
    tripwire = False
 }

-- ## CHANGE PICTURE - END ##

-- | A call that combines multiple strings to one text-call
putMultTextCall:: Identifier -- ^ The 'Call'-ID
               -> String -- ^ The 'Call'-description
               -> [String] -- ^ The texts to display
               -> String -- ^ The 'Call'-save-name
               -> Call
putMultTextCall id title texts = putMultTextCall' id title texts []

putMultTextCall':: Identifier -> String -> [String] -> [Call] -> String -> Call
putMultTextCall' _ _ [] textCalls name = combineCalls (reverse textCalls) name
putMultTextCall' id title (text : ts) textCalls name
    = putMultTextCall' id
                       title
                       ts
                       (putTextCall' id title text False "" : textCalls)
                       name

-- | A call that displays text in the textbox
putTextCall:: Identifier -- ^ The 'Call'-ID
           -> String -- ^ The 'Call'-description
           -> String -- ^ The text to display
           -> String -- ^ The 'Call'-save-name
           -> Call
putTextCall id title text = putTextCall' id title text True

putTextCall':: Identifier -> String -> String -> Bool -> String -> Call
putTextCall' ide title text dis name = Call {
    id_c = ide,
    description = title,
    initiator = -1,
    receiver = textBoxID,
    task = displayText text,
    interaction = id,
    persistence = False,
    disposable = dis,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}

putTextCall2:: Identifier -- ^ The 'Call'-ID
            -> String -- ^ The 'Call'-description
            -> String -- ^ The text to display
            -> String -- ^ The 'Call'-save-name
            -> Call
putTextCall2 id title text = putTextCall2' id title text True

putTextCall2':: Identifier -> String -> String -> Bool -> String -> Call
putTextCall2' ide title text dis name = Call {
    id_c = ide,
    description = title,
    initiator = -1,
    receiver = textBoxID,
    task = displayText' text,
    interaction = id,
    persistence = False,
    disposable = dis,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}

-- | A call that display a question in the textbox
putQuestionCall:: Identifier -- ^ The 'Call'-ID
               -> String -- ^ The 'Call'-description
               -> String -- ^ The question to display
               -> (String -> Call) -- ^ A function that depends on the user input and acts as the games reaction
               -> String -- ^ The 'Call'-save-name
               -> Call
putQuestionCall ide title text f name = Call {
    id_c = ide,
    description = title,
    initiator = -1,
    receiver = textBoxID,
    task = displayQuestion text f,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}

-- | Moves the player instantly to 'newStartIndex'
portPlayer:: String -- ^ The 'Call'-description 
          -> Offset -- ^ The new position of the player model
          -> String -- ^ The 'Call'-save-name
          -> Call
portPlayer = portObject playerModel_ID

-- | Moves an Object to 'newStartIndex'
portObject:: Identifier -- ^ The 'Object'-ID from the 'Object' that shall be moved
          -> String -- ^ The 'Call'-description
          -> Offset -- ^ The new position of the 'Object'
          -> String -- ^ The 'Call'-save-name
          -> Call
portObject pinId title newStartIndex name = Call {
    id_c = -22,
    description = title,
    initiator = -1,
    receiver = playgroundID,
    task = return,
    interaction = theTask,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}
  where
    theTask:: UpdatePlayground
    theTask pg = applyOnObject pinId (setPositionObj newStartIndex) pg


-- | Player takes an item from another object
takeItem:: String -- ^ The 'Call'-description
        -> String -- ^ The 'Item'-description
        -> Int -- ^ The 'Object' from which to take the 'Item'
        -> String -- ^ The 'Call'-save-name
        -> Call
takeItem title itemDesc id name = Call {
    id_c = -11,
    description = title,
    initiator = -1,
    receiver = playgroundID,
    task = return,
    interaction = theTask,
    persistence = False,
    disposable = True,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}
  where
    theTask:: UpdatePlayground
    theTask pg = applyOnObject id (removeItemsObj $ getItems pg)  -- remove item from old pin
               $ applyOnObject playerModel_ID (addItemsObj $ getItems pg) pg  -- add item to new pin
    getItems:: Playground -> [Item]
    getItems pg = [item | item <- inventory.head.getPins id $ pg
                        , description_item item == itemDesc ]

dropItem:: String -- ^ The 'Call'-description
        -> Item -- ^ The 'Item' to drop
        -> Int -- ^ The 'Pin'-ID of the 'Pin' that contains the 'Item' to drop
        -> String -- ^ The 'Call'-save-name
        -> Call
dropItem title item ide name = Call {
    id_c = -16,
    description = title,
    initiator = -1,
    receiver = playgroundID,
    task = theTask,
    interaction = id,
    persistence = False,
    disposable = True,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}
  where
    theTask:: UpdatePinboard
    theTask pb
        = do -- get Pin and remove the item
             let ps = getPins ide pb
             -- remove item by making it invisible
                 newPs = map (execState (alterPin' $ removeItems [item])) ps
                 adjustedItem = removeVisibility item
                 newPs' = map (execState (alterPin' $ addItems [adjustedItem])) newPs
             -- update pinboard accordingly
             (_, newPb) <- runStateT (alterPinboard $ removePins [ide]) pb
             (_, newPb2) <- runStateT (alterPinboard $ addPins newPs') newPb
             return newPb2

-- | A 'Call' that executes a '(Bool -> Call)' depending on whether the player has a certain 'Item'
chkItemCall:: String -- ^ The 'Item' that shall be checked
           -> (Bool -> Call) -- ^ The function to execute
           -> String -- ^ The 'Call'-save-name
           -> Call
chkItemCall itemDesc f name = Call {
    id_c = -14,
    description = "Check Item",
    initiator = -1,
    receiver = playgroundID,
    task = theTask,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}
  where
    theTask:: UpdatePinboard
    theTask pb
      = do let player = head $ getPins playerModel_ID pb
               decider = itemDesc `elem` map description_item (inventory player)
           executeCalls [setNested True $ f decider] pb

-- | A 'Call' that executes a '(Bool -> Call)' depending on whether the player has a multiple 'Item's
chkMultItemsCall:: [String] -- ^ The 'Item's that shall be checked
                -> ([String] -> Call) -- ^ The function to execute
                -> String -- ^ The 'Call'-save-name
                -> Call
chkMultItemsCall items f name = Call {
    id_c = -14,
    description = "Check Item",
    initiator = -1,
    receiver = playgroundID,
    task = theTask,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}
  where
    theTask:: UpdatePinboard
    theTask pb
      = do let player = head $ getPins playerModel_ID pb
               playerInv = map description_item (inventory player)
               decider = filter (`elem` playerInv) items
           executeCalls [setNested True $ f decider] pb

-- | Either displays an 'end-of-chapter'-message to the player or changes the 'Playground' to the next chapter.
chapterTransitionCall:: String -- ^ The 'Call'-description
                     -> String -- ^ The 'end-of-chapter'-text
                     -> Int -- ^ The index of the 'Playground' in the 'transitionRegister' to which the game shall 
                            --   change. If this index refers to a non-existing entry, the end-of-chapter text will be
                            --   displayed instead.
                     -> String -- ^ The 'Call'-save-name
                     -> Call
chapterTransitionCall title endText pgNumber name = Call {
    id_c = -15,
    description = title,
    initiator = -1,
    receiver = playgroundID,
    task = thisTask,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}
    where
      {-# NOINLINE thisTask #-}
      thisTask:: UpdatePinboard
      thisTask pb
        = let newPg = transitionRegister !! pgNumber
          in if unsafePerformIO $ liftA2 (==) newPg identityPg
               then executeCalls [
                      setNested True $ putTextCall (-16) title endText ""
                    ] pb
               else executeCalls [setNested True $ changePg title newPg ""] pb

-- | Moves an 'Object' step by step by adding speed to it
moveObjectCall:: String -- ^ The 'Call'-description
              -> Identifier -- ^ The ID of the target 'Object'
              -> (Int, Int) -- ^ The horizontal and vertical speed for the target 'Object'
              -> String -- ^ The 'Call'-save-name
              -> Call
moveObjectCall title targetId speed name = Call {
    id_c = -16,
    description = title,
    initiator = -1,
    receiver = playgroundID,
    task = return,
    interaction = addVelocity targetId speed,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}

-- | Remove an 'Object' from the playground
removeObjectCall:: String -- ^ The 'Call'-description
                -> Identifier -- ^ The ID of the 'Object' to remove
                -> String -- ^ The 'Call'-save-name
                -> Call
removeObjectCall title targetId name = Call {
    id_c = -17,
    description = title,
    initiator = -1,
    receiver = playgroundID,
    task = theTask,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}
    where
        theTask pb = do (_, newPb) <- runStateT (alterPinboard (removePins [targetId])) pb
                        printPinboardNoLoop newPb pb
                        return newPb

-- | Choose a call depending on the 'Objects' position
chkPositionCall:: String -- ^ The 'Call'-description
               -> Identifier -- ^ The 'Object'-ID
               -> (Offset -> Call) -- ^ The function to execute
               -> String -- ^ The 'Call'-save-name
               -> Call
chkPositionCall title targetId f name = Call {
    id_c = -18,
    description = title,
    initiator = -1,
    receiver = playgroundID,
    task = theTask,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}
    where
        theTask pb = do let player = head $ getPins targetId pb
                            playerPos = startIndex player
                        executeCalls [setNested True $ f playerPos] pb

-- | Prints the inventory a 'Pin'
showInventory:: Identifier -- ^ The ID of the 'Pin' from which to display the inventory
             -> Call
showInventory pinId = Call {
    id_c = -19,
    description = "",
    initiator = -1,
    receiver = playgroundID,
    task = theTask,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = "",
    tripwire = False
}
    where
      theTask:: UpdatePinboard
      theTask pb
          = do let pin = head $ getPins pinId pb
                   inv = show $ inventory pin
               print inv
               return pb

-- | Determine which 'Call' is executed at the hand of the visibility of an 'Item'
chkItmVisCall:: String -- ^ The 'Call'-description
             -> Item -- ^ The 'Item' to check for
             -> (Bool -> Call) -- ^ The function to execute
             -> String -- ^ The 'Call'-save-name
             -> Call
chkItmVisCall title item f name = Call {
    id_c = -20,
    description = title,
    initiator = -1,
    receiver = playgroundID,
    task = theTask,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}
    where
        theTask:: UpdatePinboard
        theTask pb
            = do let inv = concatMap inventory
                         $ getPins playerModel_ID pb
                     isVisible = and [display_item i | i <- inv, i == item]
                 executeCalls [setNested True $ f isVisible] pb

-- | Change the appearence of a 'Pin' in a loop 
changeGridInLoop:: String -- ^ The 'Call'-description
                -> (Int, Int) -- ^ The dimension of the 'Grid'
                -> [String] -- ^ The appearences of the 'Grid' which we loop through
                -> Int -- ^ The amount of steps through the loop
                -> Identifier -- ^ The ID of the 'Pin' which shall change the appearence
                -> String -- ^ The 'Call'-save-name
                -> Call
changeGridInLoop title dim (init : res) repititions pinId name
  = Call {
    id_c = -21,
    description = title,
    initiator = -1,
    receiver = playgroundID,
    task = theTask repititions res,
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}
    where
      theTask:: Int -> [String] -> UpdatePinboard
      theTask 0 _ pb = do newAppGrid <- createGrid dim init
                          (_, newPb) <- runStateT (alterPinboard 
                                      $ alterSpecificPin (changeAppearence newAppGrid) pinId) 
                                        pb
                          printPinboardNoLoop newPb pb
      theTask reps (newApp : as) pb 
        = do newAppGrid <- createGrid dim newApp
             (_, newPb) <- runStateT (alterPinboard 
                         $ alterSpecificPin (changeAppearence newAppGrid) pinId) 
                           pb
             printPinboardNoLoop newPb pb
             threadDelay 500000
             theTask (reps - 1) (as ++ [newApp]) pb
