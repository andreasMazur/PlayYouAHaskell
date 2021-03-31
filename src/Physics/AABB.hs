
module Physics.AABB where

import GHC.IO.Unsafe (unsafePerformIO)
import Data.Array.IO (MArray(getBounds))

import GameEngine.Pin (Pin(..))

{-

    Axis Aligned Bounding Boxes:

    - Not rotated boxes relative to the game axes
    - Include the object for which collisions shall be detected
    - Used to determine whether two objects overlap or not

    Source:
    https://developer.mozilla.org/en-US/docs/Games/Techniques/3D_collision_detection

-}

type Offset = (Int, Int)
type Dimension = (Int, Int)

data AABB = AABB {
    minX :: Int,
    maxX :: Int,
    minY :: Int,
    maxY :: Int,
    dimension :: (Int, Int)
}

instance Show AABB where
    show aabb = "(" ++ (show $ minX aabb) ++ 
                "," ++ (show $ maxX aabb) ++
                "," ++ (show $ minY aabb) ++
                "," ++ (show $ maxY aabb) ++ ")"

{-# NOINLINE createAABBForPin #-}
createAABBForPin:: Pin -> AABB
createAABBForPin pin = AABB {
    minX = snd.startIndex $ pin,
    maxX = (snd.startIndex $ pin) + (snd dim) - 1,
    minY = fst.startIndex $ pin,
    maxY = (fst.startIndex $ pin) + (fst dim) - 1,
    dimension = dim
 }
 where
     -- Since we only read the grids dimension (which never changes)
     -- we extract the dimension out of the IO context. No side effects
     -- expected.
     dim:: Dimension
     dim = snd.unsafePerformIO.getBounds.grid $ pin

updateAABB:: Offset -> AABB -> AABB
updateAABB (y, x) (AABB xMin xMax yMin yMax dim)
    = AABB (xMin + x) (xMax + x) (yMin + y) (yMax + y) dim

increase:: Offset -> AABB -> AABB
increase (addY, addX) (AABB xMin xMax yMin yMax dim)
    = AABB (xMin - addX) (xMax + addX) (yMin - addY) (yMax + addY) dim
