{-# LANGUAGE RankNTypes #-}

module GameEngine.Call where

import {-# SOURCE #-} GameEngine.Pinboard (Pinboard, UpdatePinboard)
import {-# SOURCE #-} GameEngine.Playground (Playground, UpdatePlayground)
import GameEngine.PinboardUtilities (addCalls)
import GameConfig.UserInterface (playgroundID)
import Data.Data (Typeable)
import GameEngine.Picture (Picture)

type Identifier = Int

{-|
    A 'Call' is describing an order for a pinboard sent by another pinboard.
    Each pinboard contains a list of calls, which will be executed at the
    start of each game-loop iteration.
-}
data Call = Call {
    id_c :: Int, -- ^ Call-ID Necessary to distinguish between calls
    description :: String, -- ^ Description will be displayed in the interaction menu
    initiator :: Identifier, -- ^ The sending pinboard
    receiver :: Identifier, -- ^ The receiving pinboard
    task :: UpdatePinboard, -- ^ The task is the pinboard-altering function
    interaction :: UpdatePlayground, -- ^ The interaction changes objects (more specific than just changing pinboards)
    persistence :: Bool, -- ^ Should the call still be displayed, after the player is no longer in contact with it?
    disposable :: Bool, -- ^ Should the call be only displayed once?
    nestedCall :: Bool, -- ^ Will the call be executed within another call?
    newPlayground :: Maybe (IO Playground), -- ^ Is this call exchanging the playground?
    newPicture :: Maybe (IO Picture), -- ^ Is this call exchanging a picture?
    executeImmediatly :: Bool, -- ^ Shall the call be executed immediatly after the player got in contact with it?
    callName :: String, -- ^ The name of the call (necessary for saving/loading reasons)
    tripwire :: Bool -- ^ Calls retrieved by collision (can be executed by any object not only by the player)
} deriving (Typeable)

instance Show Call where
    show call = callName call

instance Eq Call where
    c1 == c2 = id_c c1 == id_c c2

-- | Creates a default call
createCall:: Identifier -- ^ Call-ID
          -> String -- ^ Call-description
          -> Identifier -- ^ Receiver-pinboard
          -> UpdatePinboard -- ^ Call-task
          -> UpdatePlayground -- ^ Call-interaction
          -> Bool -- ^ Call-persistency
          -> String -- ^ Call-name
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

-- | Creates an interaction, i.e. a call that alters objects within the playground (more specific than 'createCall')
createInteraction:: Identifier -- ^ Call-ID
                 -> String -- ^ Call-description
                 -> UpdatePlayground -- ^ Call-interaction
                 -> Bool -- ^ Whether the call is disposable
                 -> String -- ^ Call-name
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

-- | Call that does not change anything
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

{-|
    Given a list of interactions which shall be removed, test whether a call is disposable or in the list of calls that 
    shall be removed.
-}
isDisposable:: Call -- ^ The call to be checked
            -> [Call] -- ^ The list of calls, which shall be removed
            -> Bool -- ^ 
isDisposable call calls
    | disposable call = call `elem` calls
    | otherwise       = False

{-|
    Call-execution describes the procedure of uploading calls from pins to the pinboard. As soon as they are uploaded,
    the 'callHandler' executes them.
-}
executeCalls:: [Call] -- ^ The calls to be executed
            -> UpdatePinboard
executeCalls calls pb = return $ addCalls calls pb

-- | See 'addCalls'
executeCalls':: Pinboard a => [Call] -> a -> a
executeCalls' = addCalls

-- | Execute multiple calls after each other by combining them to one call.
combineCalls:: [Call] -- ^ The list of calls to be combined
            -> String -- ^ Call-name
            -> Call
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

combineCalls':: [Call] -- ^ The list of calls to be combined
             -> Identifier -- ^ Call-ID
             -> Identifier -- ^ Call-initiator
             -> Identifier -- ^ Call-receiver
             -> String -- ^ Call-description
             -> Bool -- ^ Whether the call shall be executed immediatly
             -> String -- ^ Call-name
             -> Bool -- ^ Whether the call is a tripwire
             -> Call -- ^ The previous call in the call-order
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
