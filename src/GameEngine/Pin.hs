
module GameEngine.Pin where

import GameEngine.Grid (showGrid,  Grid )
import {-# SOURCE #-} GameEngine.Call ( Call )
import {-# SOURCE #-} GameEngine.Item ( Item )

type UpdatePin = Pin -> Pin
type StartIndex = (Int, Int)
type Identifier = Int

-- | A pin-value contains information about a grid's access and its
--   appearence. It is used to represent "objects" within a pinboard.
data Pin = Pin {
    grid :: Grid,
    startIndex :: StartIndex,
    id_pin :: Identifier,
    hasChanged :: Bool,
    calls_pin :: [Call],
    rigid :: Bool,
    inBackground :: Bool,
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
