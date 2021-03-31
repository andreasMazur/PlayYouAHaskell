
module Physics.MovementV2 where

import GameEngine.Object
    (getId,  Object(object, shape, velocity), UpdateObject, isRigid )
import Linear ( V2(..) )
import GameEngine.Playground
    ( Playground(objects), UpdatePlayground )
import Control.Monad.State ( execState )
import GameEngine.PinUtilities (movePin,  alterPin')
import Physics.AABB ( updateAABB, AABB(minY, maxY, minX, maxX) )
import Data.List ((\\))
import Physics.Collision (collides)
import GameConfig.UserInterface (playgroundDim)
import Physics.Gravity (objOnObj, objOnGround)
import GameEngine.Pin (id_pin)
import GameConfig.PlayerModel (playerModel_ID)

-- ## INTERFACE ##

moveObjects:: UpdatePlayground
moveObjects pg = pg { objects = map (applyVelocity pg) $ objects pg }

addVelocity:: Identifier -> (Int, Int) -> UpdatePlayground
addVelocity targetId vel2Add pg 
    = pg { objects = (objects pg \\ target) ++ alteredTarget }
    where
        target = [o | o <- objects pg, id_pin (object o) == targetId]
        alteredTarget = map (addVelocity' vel2Add) target

addVelocity':: (Int, Int) -> UpdateObject
addVelocity' (yVel, xVel) obj 
    = obj { velocity = V2 (asDoubleX + prevXVel) (asDoubleY + prevYVel) }
    where
        (V2 prevXVel prevYVel) = velocity obj
        asDoubleX = read (show xVel) :: Double
        asDoubleY = read (show yVel) :: Double

-- ## APPLY VELOCITY ##

{-
    Before shift check if:
        - object is not rigid
        - object does not collide
        - object stays in game bounds
-}

-- | Translates velocity of an object into a shift
applyVelocity:: Playground -> UpdateObject
applyVelocity pg obj
    |  not (isRigid obj)
    && inBoundaries obj shift
    && collides newObj (objects pg \\ [obj]) = newObj
    | otherwise = obj { velocity = updatedVel }
    where
        {-
            Calculating the shift and new velocity:
                - object moves max 1 char per gameloop iteration
                - each gameloop iteration friction decreases the absolute value 
                  of the velocity
                - the aabb has to move likewise as the grid
        -}
        newObj = obj { object = execState (alterPin' 
                              $ movePin shift) (object obj),  -- update position (grid)
                       shape = updateAABB shift
                             $ shape obj,  -- update position (aabb)
                       velocity = updatedVel }  -- update velocity (friction)
        (V2 xDir yDir) = velocity obj
        shift = (chooseDir yDir, chooseDir xDir)
        chooseDir vel
            | vel > 0   = 1
            | vel < 0   = -1
            | otherwise = 0
        (V2 newXVel newYVel) = V2 (reduce xDir) (reduce yDir)
        reduce vel
            | vel > 0   = vel - 1
            | vel < 0   = vel + 1
            | otherwise = vel
        updatedVel = V2 (maxVel newXVel) newYVel
        maxVel vel = if id_pin (object obj) == playerModel_ID
                       then cap vel
                       else vel
        cap vel
            | vel > 1   = 1
            | vel < -1  = -1
            | otherwise = vel

inBoundaries:: Object -> (Int, Int) -> Bool
inBoundaries obj (yAdd, xAdd)
    = foldr ((&&).withinBounds) True cornerPoints
    where
        aabb = shape obj
        cornerPoints = [ (y + yAdd, x + xAdd) | y <- [minY aabb, maxY aabb]
                                              , x <- [minX aabb, maxX aabb] ]

withinBounds:: (Int, Int) -> Bool
withinBounds (y, x) 
    = 0 < y && y < fst playgroundDim && 0 < x && x < snd playgroundDim

-- ## HANDLE V-MAX ##

maxVel:: Double
maxVel = 1.0

capVel:: V2 Double -> V2 Double
capVel (V2 xDir yDir) = V2 (cap xDir) (cap yDir)

cap:: Double -> Double
cap vel
    | vel > maxVel    = maxVel
    | vel < (-maxVel) = -maxVel
    | otherwise       = vel

-- ## MOVING ##

type Identifier = Int

move:: Identifier -> V2 Double -> UpdateObject
move id vector obj
  | getId obj == id = obj { velocity = velocity obj + vector }
  | otherwise       = obj

moveRight:: Identifier -> UpdatePlayground
moveRight id pg = pg {
  objects = map (move id (V2 1 0)) $ objects pg 
}

moveLeft:: Identifier -> UpdatePlayground
moveLeft id pg = pg {
  objects = map (move id (V2 (-1) 0)) $ objects pg 
}

-- ## JUMPING ##

jumpFunc:: Playground -> Identifier -> V2 Double -> UpdateObject
jumpFunc pg id dir obj
  | (objOnGround obj || objOnObj obj (objects pg)) && getId obj == id 
  = obj { velocity = velocity obj + dir }
  | otherwise          = obj

jumpRight:: Identifier -> UpdatePlayground
jumpRight id pg = pg { 
  objects = map (jumpFunc pg id (V2 2 (-3))) $ objects pg 
}

jumpLeft:: Identifier -> UpdatePlayground
jumpLeft id pg = pg { 
  objects = map (jumpFunc pg id (V2 (-2) (-3))) $ objects pg 
}

jumpUp:: Identifier -> UpdatePlayground
jumpUp id pg = pg { 
  objects = map (jumpFunc pg id (V2 0 (-3))) $ objects pg 
}