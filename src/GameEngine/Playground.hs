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

addObject:: Object -> IO Playground -> IO Playground
addObject newObj newPg = do pg <- newPg
                            return $ pg { objects = newObj : objects pg }

-- | Wrapperfunction for interactions, which only affect a single object
-- | 'id' : The id of the object, that shall be modified
-- | 'UpdateObject' : The function, which shall be applied on the target object
applyOnObject:: Identifier -> UpdateObject -> UpdatePlayground
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

-- TODO
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

createPlayground:: String
                -> String
                -> [Call]
                -> [(String, Dimension, StartIndex, [Call], Bool, Bool, [Item])]
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

createPlaygroundWithPins:: String -> String -> [Pin] -> [Call] -> IO Playground
createPlaygroundWithPins appearence desc pins calls
    = do pb <- createPin' appearence playgroundDim playgroundStartIndex 0 []
         return $ Playground {
            background = pb,
            objects = map createObject pins,
            identifier = playgroundID,
            calls = calls,
            description = desc
         }

createPlaygroundWithPin:: (IO Pin) -> String -> [Object] -> [Call] -> IO Playground
createPlaygroundWithPin bg desc os calls
    = do pb <- bg
         return Playground {
             background = pb,
             objects = os,
             identifier = playgroundID,
             calls = calls,
             description = desc
         }

createPlaygroundWithIds:: String
                  -> String
                  -> [Call]
                  -> [(String, Dimension, StartIndex, [Call], Bool, Bool, [Item], Identifier)]
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
