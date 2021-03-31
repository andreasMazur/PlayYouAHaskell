{-# LANGUAGE RankNTypes #-}

module GameEngine.Call where

import {-# SOURCE #-} GameEngine.Pinboard (Pinboard, UpdatePinboard)
import {-# SOURCE #-} GameEngine.Playground (Playground, UpdatePlayground)
import GameEngine.PinboardUtilities (addCalls)
import GameConfig.UserInterface (playgroundID)
import Data.Data (Typeable)
import GameEngine.Picture (Picture)

type Identifier = Int

-- | A 'Call' is describing an order for a pinboard sent by another pinboard.
--   Each pinboard contains a list of calls, which will be executed at the
--   start of each game-loop iteration.
-- | As the 'callHandler' needs to manage interactions aswell but only works
--   with 'Call's, the 'interaction'-field was added to the data type.
data Call = Call {
    -- necessary to sort calls
    id_c :: Int,
    -- Description will be displayed in the interaction menu
    description :: String,
    -- The sending pinboard
    initiator :: Identifier,
    -- The receiving pinboard
    receiver :: Identifier,
    -- The task changes pinboards
    task :: UpdatePinboard,
    -- The interaction changes objects (more specific than just changing pinboards)
    interaction :: UpdatePlayground,
    -- Should the call still be displayed, after the player is no longer in contact with it?
    persistence :: Bool,
    -- Should the call be only displayed once?
    disposable :: Bool,
    -- Will the call be executed within another call?
    nestedCall :: Bool,
    -- Is this call changing the playground?
    newPlayground :: Maybe (IO Playground),
    -- Is this call changing a picture?
    newPicture :: Maybe (IO Picture),
    -- Shall the call be executed immediatly after the player got in contact with it?
    executeImmediatly :: Bool,
    -- The name of the call (necessary for saving/loading reasons)
    callName :: String,
    -- Call can be executed by any object (not only by the player)
    tripwire :: Bool
} deriving (Typeable)

instance Show Call where
    show call = callName call

instance Eq Call where
    c1 == c2 = id_c c1 == id_c c2

createCall:: Identifier
          -> String
          -> Identifier
          -> UpdatePinboard
          -> UpdatePlayground
          -> Bool
          -> String
          -> Call
createCall id title re updatePinboard updatePlayground pers name = Call {
    id_c = id,
    description = title,
    initiator = -1,
    receiver = re,
    task = updatePinboard,
    interaction = updatePlayground,
    persistence = pers,
    disposable = True,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}

createInteraction:: Identifier
                 -> String
                 -> UpdatePlayground
                 -> Bool
                 -> String
                 -> Call
createInteraction id title updatePlayground disp name = Call {
    id_c = id,
    description = title,
    initiator = -1,
    receiver = playgroundID,
    task = return,
    interaction = updatePlayground,
    persistence = False,
    disposable = disp,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = name,
    tripwire = False
}

identityCall:: Call
identityCall = Call {
    id_c = -1,
    description = "Identity call",
    initiator = -1,
    receiver = -1,
    task = return,
    interaction = id,
    persistence = False,
    disposable = True,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = "identityCall",
    tripwire = False
}

setPersistence:: Bool -> Call -> Call
setPersistence status call = call { persistence = status }

setInitiator:: Identifier -> Call -> Call
setInitiator id call = call { initiator = id }

setNested:: Bool -> Call -> Call
setNested value call = call { nestedCall = value }

adjustExecutionPoint:: Bool -> Call -> Call
adjustExecutionPoint value call = call { executeImmediatly = value }

renameCall:: String -> Call -> Call
renameCall name call = call { description = name }

setTripwire:: Bool -> Call -> Call
setTripwire value call = call { tripwire = value }

-- | Tests if an interaction is, for a given list of interactions which shall
--   be removed, disposable 
isDisposable:: Call -> [Call] -> Bool
isDisposable call calls
    | disposable call = call `elem` calls
    | otherwise       = False

-- | 'Call-execution' describes the uploading of the calls from pins to the
--   pinboard. As soon as they are uploaded, the calls-handler executes them.
executeCalls:: [Call] -> UpdatePinboard
executeCalls calls pb = return $ addCalls calls pb

executeCalls':: Pinboard a => [Call] -> a -> a
executeCalls' = addCalls

-- | Execute multiple calls after each other by combining them to one call.
combineCalls:: [Call] -> String -> Call
combineCalls (call : cs) name
    = combineCalls' cs
                    (id_c call)
                    (initiator call)
                    (receiver call)
                    (description call)
                    (executeImmediatly call)
                    name
                    (tripwire call)
                    call

combineCalls':: [Call]
             -> Identifier
             -> Identifier
             -> Identifier
             -> String
             -> Bool
             -> String
             -> Bool
             -> Call
             -> Call
combineCalls' [] _ _ _ _ _ _ _ prevCall = prevCall
combineCalls' (call : cs) id init re desc immediate name trap prevCall
    = combineCalls' cs id init re desc immediate name trap Call {
        id_c = id,
        description = desc,
        initiator = init,
        receiver = re,
        task = chainUpdatePinboard,
        interaction = interaction call.interaction prevCall,
        persistence = persistence call || persistence prevCall,
        disposable = disposable call && disposable prevCall,
        nestedCall = nestedCall call || nestedCall prevCall,
        newPlayground = newPlayground call,
        newPicture = Nothing,
        executeImmediatly = immediate,
        callName = name,
        tripwire = trap
    }
    where
        -- | Calls, which refer a different receiver than the first listed call,
        --   will be executed as nested calls. Hence, all calls that refer
        --   a different receiver than the first call, will be executed after
        --   all calls which refer the same receiver as the first call.
        chainUpdatePinboard:: UpdatePinboard
        chainUpdatePinboard pb
            | receiver call /= re = do newPb <- task prevCall pb
                                       executeCalls [setNested True call] newPb
            | otherwise           = do newPb <- task prevCall pb
                                       task call newPb
