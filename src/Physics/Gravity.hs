
module Physics.Gravity where

import Linear.V2 ( V2(..) )
import Data.List ((\\))

import GameEngine.Playground ( Playground(..), UpdatePlayground )
import GameEngine.Object (isRigid, Object(..), UpdateObject )

import GameConfig.UserInterface (ground )

import Physics.AABB ( AABB(..) )

objOnGround:: Object -> Bool
objOnGround obj = let yMax = maxY.shape $ obj
                  in yMax == ground

objOnObj:: Object -> [Object] -> Bool
objOnObj obj os = foldl (||) False $ map (objOnObj' obj) 
                                   $ os \\ [obj]

objOnObj':: Object -> Object -> Bool
objOnObj' obj1 obj2 = let yMaxObj1 = maxY.shape $ obj1
                          yMinObj2 = minY.shape $ obj2
                          xMinObj1 = minX.shape $ obj1
                          xMaxObj1 = maxX.shape $ obj1
                          xMinObj2 = minX.shape $ obj2
                          xMaxObj2 = maxX.shape $ obj2
                      in yMaxObj1 + 1 == yMinObj2
                         && xMinObj1 <= xMaxObj2
                         && xMinObj2 <= xMaxObj1

gravity:: Playground -> Object -> V2 Double
gravity pg obj
    | objOnGround obj = V2 0 0
    | isRigid obj = V2 0 0
    | objOnObj obj $ objects pg = V2 0 0
    | otherwise = V2 0 1

addGravity:: Playground -> UpdateObject
addGravity pg obj = obj { velocity = velocity obj + gravity pg obj }

-- | Remember: Gravity vector is added before any movement vector
applyGravity:: UpdatePlayground
applyGravity pg = pg { objects = map (addGravity pg) $ objects pg }
