
module GameEngine.Object where

import Linear.V2 ( V2(..) )
import GHC.Float ( int2Double )
import Control.Monad.State (execState,  put, get, modify,  State )


import GameEngine.PinUtilities (alterPin', setPosition, removeItems, addItems,  setStatus )
import {-# SOURCE #-} GameEngine.Call ( Call )

import Physics.AABB (createAABBForPin,  AABB(..) )
import GameEngine.Pin ( Pin(..) )
import {-# SOURCE #-} GameEngine.Item (Item)

type UpdateObject = Object -> Object
type Identifier = Int
type Offset = (Int, Int)

data Object = Object {
    object :: Pin,
    shape :: AABB,
    mass :: Double,
    velocity :: V2 Double,
    restitution :: Double
}

instance Show Object where
    show obj = "<Object>\n"
            ++ (show $ object obj)
            ++ "<Shape>" ++ (show $ shape obj) ++ "</Shape>\n"
            ++ "<Mass>" ++ (show $ mass obj) ++ "</Mass>\n"
            ++ "<Velocity>" ++ (show $ velocity obj) ++ "</Velocity>\n"
            ++ "<Restitution>" ++ (show $ restitution obj) ++ "</Restitution>\n"
            ++ "</Object>\n"

showMultipleObjects:: [Object] -> String
showMultipleObjects [] = ""
showMultipleObjects (o : os) = show o ++ showMultipleObjects os

instance Eq Object where
    o1 == o2 = (object o1) == (object o2)

-- ## GET-FUNCTIONS ##

getPosition:: Object -> V2 Int
getPosition obj = V2 (snd pos) (fst pos)
    where
        pos:: (Int, Int)
        pos = startIndex.object $ obj

getId:: Object -> Identifier
getId = id_pin.object

getCalls:: Object -> [Call]
getCalls = calls_pin.object

isRigid:: Object -> Bool
isRigid = rigid.object

isInBackground:: Object -> Bool
isInBackground = inBackground.object


-- ## SET FUNCTIONS ##

exchangePin:: Pin -> UpdateObject
exchangePin pin obj = obj { object = pin }

setPositionObj:: Offset -> UpdateObject
setPositionObj offset obj 
  = obj { object = execState (alterPin' $ setPosition offset) $ object obj }

addItemsObj:: [Item] -> UpdateObject
addItemsObj items obj = obj { object = execState (alterPin' $ addItems items) $ object obj }

removeItemsObj:: [Item] -> UpdateObject
removeItemsObj items obj = obj { object = execState (alterPin' $ removeItems items) $ object obj }

-- ## MISC ##

createObject:: Pin -> Object
createObject pin
    = Object {
           object = pin,
           shape = aabb,
           mass = int2Double $ fst (dimension aabb) * snd (dimension aabb),
           velocity = 0,
           restitution = int2Double $ fst (dimension aabb) * snd (dimension aabb)
         }
      where
        aabb:: AABB
        aabb = createAABBForPin pin

-- | TODO
createObject':: Pin -> Double -> Double -> Double -> Object
createObject' pin ma _ res
    = Object {
           object = pin,
           shape = aabb,
           mass = ma,
           velocity = 0,
           restitution = res
         }
      where
        aabb:: AABB
        aabb = createAABBForPin pin


-- | Updates an object
alterObject:: UpdateObject -> State Object Object
alterObject g
    = do initialObj <- get
         modify g
         newObj <- get
         if initialObj == newObj
           then return initialObj
           else do let newObj' = exchangePin (setStatus True $ object newObj)
                                             newObj
                   put newObj'
                   return initialObj
