{-# LANGUAGE RankNTypes #-}

module GameEngine.Playground where

import GameEngine.PinUtilities (createPin, createPin',  setStatus )
import {-# SOURCE #-} GameEngine.Call (setInitiator,  Call )
import GameEngine.Object
    ( showMultipleObjects,
      createObject,
      getId,
      Object(..),
      createObject,
      alterObject,
      UpdateObject )
import qualified GameEngine.Pinboard as Pb ( Pinboard(..) )
import GameEngine.Pin ( Pin(..) )

import GameConfig.UserInterface (
    playgroundStartIndex,
    playgroundID,
    playgroundDim
 )
import Control.Monad.State (runState )
import {-# SOURCE #-} GameEngine.StandardCalls (updateCalls)
import GameConfig.PlayerModel (playerModel_ID)
import Data.List ((\\))
import GameEngine.Item (Item)

type UpdatePlayground = Playground -> Playground
type Identifier = Int
type Dimension = (Int, Int)
type StartIndex = (Int, Int)

{-|
    A 'Playground' represents an environment in which physics actively
    affect the elements the environment contains. The purpose of the
    'Playground' environment is that it shall represent the main game,
    in which the player moves and interacts with the elements in the
    environment.
-}
data Playground = Playground {
    background :: Pin,
    objects :: [Object],
    identifier :: Int,
    calls :: [Call],
    description :: String
}

instance Show Playground where
    show pg =  "<Playground>\n"
            ++ (show $ background pg)
            ++ (showMultipleObjects $ objects pg)
            ++ "<Identifier>" ++ (show $ identifier pg) ++ "</Identifier>\n"
            ++ "<Calls>" ++ (show $ calls pg) ++ "</Calls>\n"
            ++ "<Description>" ++ (show $ description pg) ++ "</Description>\n"
            ++ "</Playground>\n"

instance Eq Playground where
    p1 == p2 = (objects p1) == (objects p2)
                && (identifier p1) == (identifier p2)
                && (description p1) == (description p2)


-- ## MISC ##

-- | Add an 'Object' to the 'Playground'
addObject:: Object -- ^ The 'Object' to add
         -> IO Playground  -- ^ The 'Playground' in which the 'Object' shall be added
         -> IO Playground
addObject newObj newPg = do pg <- newPg
                            return $ pg { objects = newObj : objects pg }

-- | Wrapperfunction for interactions, which only affect a single object
applyOnObject:: Identifier -- ^ The ID of the 'Object', that shall be modified
             -> UpdateObject -- ^ The function, which shall be applied on the target 'Object'
             -> UpdatePlayground
applyOnObject id f pg = pg { objects = map onObject $ objects pg }
    where
        onObject:: UpdateObject
        onObject obj
            | (id_pin.object $ obj) == id = snd $ runState (alterObject f) obj
            | otherwise = obj

-- ## SET METHODS ##

setStatusPl:: Bool -> UpdatePlayground
setStatusPl status playground = playground {
    background = setStatus status $ background playground
}

setCalls:: [Call] -> Playground -> Playground
setCalls cs pg = pg { calls = map (setInitiator $ identifier pg) cs }

displayPinCalls:: [Call] -> UpdatePlayground
displayPinCalls cs pg
     = pg { calls = updateCalls cs : calls pg }

exchangePinboard:: Pin -> UpdatePlayground
exchangePinboard pin playground = playground { background = pin }

exchangeObjects:: [Object] -> UpdatePlayground
exchangeObjects os pg = pg { objects = os }

exchangePins:: [Pin] -> UpdatePlayground
exchangePins pins playground
    = playground { objects = map (adjustVel . createObject) pins }
    where
        prevObjects = objects playground
        adjustVel obj = adjustVel' obj prevObjects
        adjustVel' obj os
            = case os of []           -> obj
                         (o : others) -> if o == obj
                                           then obj { velocity = velocity o }
                                           else adjustVel' obj others

exchangeId:: Int -> Playground -> Playground
exchangeId id playground = playground { identifier = id }

-- ## GET METHODS ##

getObject:: Int -> Playground -> Object
getObject id pg = head [obj | obj <- objects pg, getId obj == id]

getPlayer:: Playground -> Object
getPlayer = getObject playerModel_ID

-- ## INSTANTIATION ##

-- | Creates a 'Playground' and its 'Object's
createPlayground:: String -- ^ The appearence of the 'Playground'
                -> String -- ^ The name of the 'Playground'
                -> [Call] -- ^ The initial 'Call's the 'Playground' shall contain
                -> [(String, Dimension, StartIndex, [Call], Bool, Bool, [Item])] -- ^ Information for the 'Objects'
                                                                                 --   contained in the 'Playground
                -> IO Playground
createPlayground appearence desc calls pinInformation
    = do let ids = [0..length pinInformation] \\ [playerModel_ID]
             io_ps = [ createPin str dim si id cs fixed inBg items
                     | ((str, dim, si, cs, fixed, inBg, items), id) <- zip pinInformation ids]
         ps <- sequence io_ps
         pb <- createPin' appearence playgroundDim playgroundStartIndex 0 []
         return $ Playground {
            background = pb,
            objects = map createObject ps,
            identifier = playgroundID,
            calls = calls,
            description = desc
         }

-- | Creates a 'Playground'
createPlaygroundWithPins:: String -- ^ The appearence of the 'Playground'
                        -> String -- ^ The name of the 'Playground'
                        -> [Pin] -- ^ The 'Pin's that shall be converted to 'Object's which are then contained in the
                                 --   'Playground'
                        -> [Call] -- ^ The initial 'Call's the 'Playground' shall contain
                        -> IO Playground
createPlaygroundWithPins appearence desc pins calls
    = do pb <- createPin' appearence playgroundDim playgroundStartIndex 0 []
         return $ Playground {
            background = pb,
            objects = map createObject pins,
            identifier = playgroundID,
            calls = calls,
            description = desc
         }

-- | Creates a 'Playground' with a given 'Pin' as the background
createPlaygroundWithPin:: (IO Pin) -- ^ The 'Pin' used as a background
                       -> String -- ^ The name of the 'Playground'
                       -> [Object] -- ^ The 'Object's contained in the 'Playground'
                       -> [Call] -- ^ The initial 'Call's the 'Playground' shall contain
                       -> IO Playground
createPlaygroundWithPin bg desc os calls
    = do pb <- bg
         return Playground {
             background = pb,
             objects = os,
             identifier = playgroundID,
             calls = calls,
             description = desc
         }

-- | Creates a 'Playground' while considering the given ID's for the 'Object's
createPlaygroundWithIds:: String -- ^ The appearence of the 'Playground'
                       -> String -- ^ The name of the 'Playground'
                       -> [Call] -- ^ The initial 'Call's the 'Playground' shall contain
                       -> [(String, Dimension, StartIndex, [Call], Bool, Bool, [Item], Identifier)] -- ^ Information for
                                                                                                    --   the 'Objects'
                                                                                                    --   contained in
                                                                                                    --   the
                                                                                                    --   'Playground'
                       -> IO Playground
createPlaygroundWithIds appearence desc calls pinInformation
    = do let io_ps = [ createPin str dim si id cs fixed inBg items
                     | (str, dim, si, cs, fixed, inBg, items, id) <- pinInformation]
         ps <- sequence io_ps
         pb <- createPin' appearence playgroundDim playgroundStartIndex 0 []
         return $ Playground {
            background = pb,
            objects = map createObject ps,
            identifier = playgroundID,
            calls = calls,
            description = desc
         }

instance Pb.Pinboard Playground where
    background = background
    exchangeBackground = exchangePinboard
    pins = map object.objects
    exchangePins = exchangePins
    id_pb = identifier
    exchange_id_pb = exchangeId
    setStatus = setStatusPl
    setCalls = setCalls
    getCalls = calls
    description_pb = description
