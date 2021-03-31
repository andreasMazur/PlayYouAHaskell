
module Physics.Collision where

import GameEngine.Object (isInBackground,  Object(..) )

import Physics.AABB (increase,  AABB(..) )
import Data.List ((\\))

type Offset = (Int, Int)

{-

    SAT - Seperating Axis Theorem:

    'Two polygons do not intersect if you can draw a line in
    between them.' 
    
    Source:
    https://wiki.delphigl.com/index.php/Tutorial_Separating_Axis_Theorem

-}

-- | COLLISION:
--   Determines for two objects if they collide.
--   Do the ranges (maxX aabb1 - minX aabb1) and (maxX aabb2 - minX aabb2)
--   overlap in any dimension?
--
--   Source for testing-formula:
--   https://developer.mozilla.org/en-US/docs/Games/Techniques/3D_collision_detection
collision:: AABB -> AABB -> Bool
collision aabb1 aabb2
    =  (minX aabb1 <= maxX aabb2 && minX aabb2 <= maxX aabb1)
    && (minY aabb1 <= maxY aabb2 && minY aabb2 <= maxY aabb1)

-- | Tests if 'obj' collides with one object out of 'os'
-- | Returns:
--     'Bool' : A truth value which tells if every object from the list
--              above is in the background
collides:: Object -> [Object] -> Bool
collides obj os
    = and [isInBackground o | o <- collidesWith (0, 0) obj os]

-- | Returns each object out of 'os', which collides with 'obj'
collidesWith:: Offset -> Object -> [Object] -> [Object]
collidesWith plus obj os = [o | o <- consideredObjects, twoCollide plus obj o]
    where
        consideredObjects:: [Object]
        consideredObjects = os \\ [obj]

-- | Tests if two objects collide
twoCollide::  Offset -> Object -> Object -> Bool
twoCollide plus obj1 obj2
    = collision (increase plus.shape $ obj1) (increase plus.shape $ obj2)

