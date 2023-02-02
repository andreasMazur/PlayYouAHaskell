
module GameEngine.Pin where

import GameEngine.Grid (showGrid,  Grid )
import {-# SOURCE #-} GameEngine.Call ( Call )
import {-# SOURCE #-} GameEngine.Item ( Item )

type UpdatePin = Pin -> Pin
type StartIndex = (Int, Int)
type Identifier = Int


{-|
    A 'Pin' is used to represent an element in PYaH.
-}
data Pin = Pin {
    -- | The appearence of the element is given by a 'Grid'
    grid :: Grid,
    -- | The start index (top left corner of 'Grid') tells where the element is located in the parent 'Pinboard'
    startIndex :: StartIndex,
    -- | Simple integer ID to distinguish the element from other elements in the 'Pinboard'
    id_pin :: Identifier,
    -- | A flag to indicate that the element has changed and thus needs to be updated by the game loop
    hasChanged :: Bool,
    -- | A set of 'Call's that can be uploaded from the 'Pin' to the 'Pinboard' (this will execute the 'Call')
    calls_pin :: [Call],
    -- | A flag to indicate that this element does not move
    rigid :: Bool,
    -- | A flag to indicate that this element is located in the background (no collision)
    inBackground :: Bool,
    -- | The inventory of the element
    inventory :: [Item]
}

instance Show Pin where
    show pin = "<Pin>\n" 
           ++ showGrid (grid pin)
           ++ "<StartIndex>" ++ (show $ startIndex pin) ++ "</StartIndex>\n"
           ++ "<IdPin>" ++ (show $ id_pin pin) ++ "</IdPin>\n"
           ++ "<HasChanged>" ++ (show $ hasChanged pin) ++ "</HasChanged>\n"
           ++ "<CallsPin>" ++ (show $ calls_pin pin) ++ "</CallsPin>\n"
           ++ "<Rigid>" ++ (show $ rigid pin) ++ "</Rigid>\n"
           ++ "<InBackground>" ++ (show $ inBackground pin) ++ "</InBackground>\n"
           ++ "<Inventory>" ++ (show $ inventory pin) ++ "</Inventory>\n"
           ++ "</Pin>\n"


instance Eq Pin where
    pin == pin' =  id_pin pin     == id_pin pin'
                && startIndex pin == startIndex pin'
