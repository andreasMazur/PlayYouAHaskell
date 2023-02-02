{-# LANGUAGE RankNTypes #-}

module GameEngine.CallHandler (
    callHandler
) where

import Control.Monad.State (put, liftIO, modify, get,  StateT )

import GameEngine.GUI ( GUI(..) )
import GameEngine.Call (identityCall, Call(..) )
import GameEngine.Playground (objects, calls, UpdatePlayground )
import qualified GameEngine.Picture as Pic
import GameEngine.Pinboard (id_pb, Pinboard(setCalls))
import GameEngine.PinboardUtilities (addCalls, getPins)
import GameConfig.PlayerModel (playerModel_ID)
import GameEngine.Pin ( Pin(inventory, id_pin) )
import GameEngine.Item (display_item, interaction_item, Item)
import GameConfig.UserInterface (ground, debugWindowID, multiUseBoxID, playgroundID)
import GameEngine.Menu (updateIBoxCalls)
import Data.Maybe (isJust)
import GameEngine.Textbox (displayText)
import GameEngine.Picture (Picture)
import GameEngine.Object ( Object(object, shape, velocity) )
import Physics.Gravity (objOnObj, objOnGround)
import Physics.MovementV2 (inBoundaries)
import Physics.Collision ( collides )
import Data.List ((\\))
import Physics.AABB ( AABB(maxY) )

-- | The call-handler acts as intermediary for the instructions sent by the pinboards of the gui.
callHandler:: StateT GUI IO GUI
callHandler
    = do initGui <- get
         -- get all calls
         let cs = retreiveCalls initGui
         -- get replacement calls
             replacementsPg = [call | call <- cs
                                    , isJust $ newPlayground call]
             replacementsPic = [call | call <- cs
                                     , isJust $ newPicture call]
         -- execute interactions
         modify $ executeInteractions cs
         -- execute calls
         guiInterDone <- get
         intermediateGui <- liftIO $ executeCalls cs guiInterDone
         let newCs = [call | call <- retreiveCalls intermediateGui
                           , nestedCall call
                           , call `notElem` cs]
         put intermediateGui
         -- execute exchange-calls
         exchangePg replacementsPg
         exchangePic replacementsPic
         -- reset all calls
         modify $ resetCalls newCs
         return initGui

-- | Exchanges the playground
exchangePg:: [Call] -- ^ The calls that contain the new playgrounds
          -> StateT GUI IO GUI
exchangePg [] = do gui <- get
                   put gui
                   return gui
exchangePg (call : cs) = do gui <- get
                            let (Just newPlg) = newPlayground call
                            newPg <- liftIO newPlg
                            put gui { playground = newPg }
                            exchangePg cs

-- | Exchanges the pinboard
exchangePic:: [Call] -- ^ The calls that contain the new picture
           -> StateT GUI IO GUI
exchangePic [] = do gui <- get
                    put gui
                    return gui
exchangePic (call : cs) = do gui <- get
                             let (Just newPict) = newPicture call
                             newPic <- liftIO newPict
                             put gui { windows = insertNewPic newPic
                                               $ windows gui }
                             exchangePic cs

-- | Exchanges the correct picture with a new one in a list of pictures
insertNewPic:: Picture -- ^ The picture to insert
            -> [Picture] -- ^ A list of pictures
            -> [Picture]
insertNewPic newPic [] = [newPic]
insertNewPic newPic (oldPic : ps)
    | Pic.identifier newPic == Pic.identifier oldPic = newPic : ps
    | otherwise = oldPic : insertNewPic newPic ps

-- ## RESET CALLS ##

-- | Resets all call-lists from each window of the GUI
resetCalls:: [Call] -- ^ The new calls that shall exist after the reset
          -> GUI -- ^ The GUI from which the windows's calls shall be resetted
          -> GUI
resetCalls nestedCalls gui = gui {
    playground = head $ assignCalls defaultCalls [playground gui],
    windows = assignCalls defaultCalls $ windows gui
}
    where
        -- DEFAULT CALLS: Calls are made every iteration.
        defaultCalls:: [Call]
        defaultCalls = [displayItems] ++ nestedCalls

        -- Default call no. 1 : Display player items
        displayItems:: Call
        displayItems = Call {
                id_c = -100,
                description = "display Items",
                initiator = playgroundID,
                receiver = multiUseBoxID,
                task = updateIBoxCalls [] calls,
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
        calls:: [Call]
        calls = [call | call <- map interaction_item playerItems
                      , call /= identityCall ]
        playerItems:: [Item]
        playerItems
            | player /= [] = [item | item <- inventory.head $ player
                                   , display_item item]
            | otherwise    = []
        player:: [Pin]
        player = getPins playerModel_ID $ playground gui

        debugMsgCall:: Call
        debugMsgCall = Call {
                id_c = -101,
                description = "Debug message",
                initiator = playgroundID,
                receiver = debugWindowID,
                task = displayText dbgMsg,
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
        pg = playground gui
        dbgMsg:: String
        dbgMsg = show [( maxY.shape $ obj
                       , ground
                       , objOnGround obj
                       , objOnObj obj $ objects $ playground gui
                       , velocity obj
                       , inBoundaries obj (0, 0)
                       , collides obj (objects pg \\ [obj]) ) | obj <- objects pg
                                      , id_pin (object obj) == playerModel_ID]

        -- Add more default calls below here

-- | Assign a list of calls to their correct initiator pinboards
assignCalls:: Pinboard a => [Call] -- ^ The calls to assign
                         -> [a] -- ^ The pinboards to which the calls shall be assigned
                         -> [a]
assignCalls _ [] = []
assignCalls calls (pb : pbs)
    = assignCallsHelp calls (setCalls [] pb) : assignCalls calls pbs

-- | Assign a list of calls to one pinboard, if the pinboard is the initiator
assignCallsHelp:: Pinboard a => [Call] -- ^ The calls to assign
                             -> a -- ^ The pinboard to which the calls shall be assigned
                             -> a
assignCallsHelp [] pb = pb
assignCallsHelp (call : cs) pb
    | id_pb pb == initiator call = assignCallsHelp cs $ addCalls [call] pb
    | otherwise                  = assignCallsHelp cs pb

-- ## GET CALLS ##

-- | Gathers all calls from each window of the GUI
retreiveCalls:: GUI -- ^ The GUI from which the calls should be gathered
             -> [Call]
retreiveCalls gui = (calls.playground $ gui) ++ concatMap Pic.calls (windows gui)

-- ## INTERACTIONS-HANDLING ##

{-|
    Exchanges the playground of the gui with an updated playground,
    on which the interactions have been applied to.
-}
executeInteractions:: [Call] -- ^ The interactions to be applied
                   -> GUI -- ^ The GUI to be modified
                   -> GUI
executeInteractions calls gui = gui { playground = applyInteractions calls $ playground gui }

-- | Applies all interactions onto the playground
applyInteractions:: [Call] -- ^ The interactions to be applied
                 -> UpdatePlayground
applyInteractions cs pg = foldl (flip interaction) pg cs

-- ## CALL-HANDLING ##

-- | Initiates the execution of the calls
executeCalls:: [Call] -- ^ The (pin-)calls to be applied
            -> GUI -- ^ The GUI, which gets modified
            -> IO GUI
executeCalls calls gui
    = do [newPlayground] <- applyCalls calls [playground gui]
         newWindows <- applyCalls calls $ windows gui
         return gui {
             playground = newPlayground,
             windows = newWindows
         }

-- | Replaces the old windows with new windows
applyCalls:: Pinboard a => [Call] -- ^ The (pin-)calls to be applied
                        -> [a] -- ^ The pinboards to modify
                        -> IO [a]
applyCalls [] windows = return windows
applyCalls (call : cs) windows
    = do newWindows <- sequence.callPbs call $ windows
         applyCalls cs newWindows

{-|
    Applies the call onto a pinboard, if the pinboards ID corresponds
    to the receiver of the call.
-}
callPbs:: Pinboard a => Call -- ^ The call to be applied
                     -> [a] -- ^ The list of considered pinboards
                     -> [IO a]
callPbs _ [] = []
callPbs call (pb : pbs)
    | id_pb pb == receiver call = task call pb : callPbs call pbs
    | otherwise                 = return pb : callPbs call pbs

