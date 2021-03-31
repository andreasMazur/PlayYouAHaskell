{-# LANGUAGE RankNTypes #-}

module Physics.PhysicsHandler (
    applyPhysics
) where

import Control.Monad.State ( modify, put, liftIO, get,  StateT(..) )

import GameEngine.Playground (displayPinCalls,  getObject,  Playground(..), UpdatePlayground )
import GameEngine.Object (object, getCalls)
import GameEngine.GUI ( GUI(..) )

import GameConfig.PlayerModel ( playerModel_ID )

import Physics.MovementV2 ( moveObjects )
import Physics.Gravity ( applyGravity )
import Physics.Collision ( collidesWith )
import GameEngine.Call (tripwire, executeCalls', executeImmediatly, Call)
import GameEngine.Pin (inBackground, id_pin)

-- | The altering-function for the playground, which acts as a interface for
--   the actual game loop to the physics engine.
applyPhysics:: UpdatePlayground -> StateT GUI IO GUI
applyPhysics g = do gui <- get
                    newPg <- liftIO $ runStateT (physicsUpdate g)
                                    $ playground gui
                    put gui { playground = snd newPg }
                    return gui

physicsUpdate:: UpdatePlayground -> StateT Playground IO Playground
physicsUpdate g
    = do initialPg <- get
         -- apply user interaction
         modify g
         -- add gravity to the objects velocities
         modify applyGravity
         -- move the objects
         modify moveObjects
         -- retreive interactions/pin-calls of colliding objects
         pg <- get
         let cs = tripwires pg
             toDisplay = [call | call <- cs
                               , not.executeImmediatly $ call]
             toExecute = [call | call <- cs
                               , executeImmediatly call]
         modify $ displayPinCalls toDisplay
         modify $ executeCalls' toExecute
         -- store the result
         pg <- get
         put pg
         return initialPg


tripwires:: Playground -> [Call]
tripwires pg = tripwiresToConsider
    where
        allIdsExceptUser = [ id_pin.object $ obj
                           | obj <- objects pg
                           , (id_pin.object) obj /= playerModel_ID
                           , not.inBackground.object $ obj ]
        userTripwires = retreiveCalls pg playerModel_ID
        otherTripwires = unique $ concatMap (retreiveCalls pg) allIdsExceptUser
        tripwiresToConsider = userTripwires ++ filter tripwire otherTripwires

retreiveCalls:: Playground -> Int -> [Call]
retreiveCalls pg id
    = concatMap getCalls (collidesWith (1, 1) (getObject id pg)
    $ objects pg)

unique:: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (x /=) xs)
