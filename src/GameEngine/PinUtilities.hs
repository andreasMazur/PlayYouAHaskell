module GameEngine.PinUtilities where

import Control.Monad.State (State, liftIO, put, modify, get,  StateT )

import GameEngine.Pin (UpdatePin,  Pin(..) )
import GameEngine.Grid (Grid,  createGrid, displayGrid )
import {-# SOURCE #-} GameEngine.Call (isDisposable,  setPersistence, Call )
import {-# SOURCE #-} GameEngine.Item (Item)
import Data.List ((\\))
import System.Console.ANSI

type StartIndex = (Int, Int)
type Dimension = (Int, Int)
type Offset = (Int, Int)
type Identifier = Int

-- ## SET-FUNCTIONS ## (Only execute in combination with alterPin resp. alterPin')

setPinCallsPersistence:: [Bool] -> UpdatePin
setPinCallsPersistence values pin = pin { calls_pin = newCalls }
  where
      newCalls:: [Call]
      newCalls = [ setPersistence value call 
                 | (call, value) <- zip (calls_pin pin) values ]

setStatus:: Bool -> UpdatePin
setStatus status pin = pin { hasChanged = status }

setPosition:: StartIndex -> UpdatePin
setPosition newPos pin = pin { startIndex = newPos }

setId:: Identifier -> UpdatePin
setId newId pin = pin { id_pin = newId }

addItems:: [Item] -> UpdatePin
addItems items pin = pin { inventory = inventory pin ++ items }

removeItems:: [Item] -> UpdatePin
removeItems items pin = pin { inventory = inventory pin \\ items }

removeCalls:: [Call] -> UpdatePin
removeCalls inters pin = pin { calls_pin = leftCalls }
  where
    leftCalls:: [Call]
    leftCalls = [ call | call <- calls_pin pin
                       , not $ isDisposable call inters ]

-- ## MISC ##

-- | Create grid-information
createPin:: String 
         -> Dimension
         -> StartIndex
         -> Identifier 
         -> [Call] 
         -> Bool 
         -> Bool 
         -> [Item] 
         -> IO Pin
createPin appearence dimension startIndex id calls fixed inBg inventory
  = do grid <- createGrid dimension appearence
       return $ Pin {
        grid = grid,
        startIndex = startIndex,
        id_pin = id,
        hasChanged = True,
        calls_pin = calls,
        rigid = fixed,
        inBackground = inBg,
        inventory = inventory
       }

createPin':: String -> Dimension -> StartIndex -> Identifier -> [Call] -> IO Pin
createPin' appearence dimension startIndex id calls
  = createPin appearence dimension startIndex id calls True False []

createPinWithGrid:: (IO Grid)
                 -> StartIndex
                 -> Identifier 
                 -> [Call]
                 -> [Item]
                 -> Bool
                 -> Bool
                 -> IO Pin
createPinWithGrid givenGrid startIndex id calls items rig inBg 
    = do g <- givenGrid
         return Pin {
          grid = g,
          startIndex = startIndex,
          id_pin = id,
          hasChanged = True,
          calls_pin = calls,
          rigid = rig,
          inBackground = inBg,
          inventory = items
         }

-- | Alters a pin.
-- | 'UpdatePin' : The function used to modify the pin
alterPin:: UpdatePin -> StateT Pin IO Pin
alterPin f = do initialPin <- get
                modify f
                pin <- get
                if initialPin == pin
                  then return initialPin
                  else do let newPin = pin { hasChanged = True }
                          put newPin  -- store the new pin
                          return initialPin

-- | Alters a pin.
-- | 'UpdatePin' : The function used to modify the pin
alterPin':: UpdatePin -> State Pin Pin
alterPin' f = do initialPin <- get
                 modify f
                 pin <- get
                 if initialPin == pin
                   then return initialPin
                   else do let newPin = pin { hasChanged = True }
                           put newPin  -- store the new pin
                           return initialPin

-- | Prints a pin.
-- | 'Offset' : An offset given by the pin's pinboard
printPin:: Offset -> StateT Pin IO Pin
printPin (yOffset, xOffset)
    = do pin <- get
         if hasChanged pin
           then do let (y, x) = startIndex pin
                   liftIO $ displayGrid (grid pin) (y + yOffset, x + xOffset)
                   put pin { hasChanged = False }
                   return pin
           else return pin

-- | TODO
printPinWithColor:: Offset -> StateT Pin IO Pin
printPinWithColor (yOffset, xOffset)
    = do pin <- get
         if hasChanged pin
           then do let (y, x) = startIndex pin
                   liftIO $ setSGR [SetColor Foreground Dull Blue]
                   liftIO $ displayGrid (grid pin) (y + yOffset, x + xOffset)
                   liftIO $ setSGR [Reset]
                   put pin { hasChanged = False }
                   return pin
           else return pin

-- | Forces to print the pin.
-- | 'Pin' : The pin that shall be printed.
forcePrintPin:: UpdatePin
forcePrintPin pin = pin { hasChanged = True }

-- | Move the pin for an offset (y, x)
movePin:: Offset -> UpdatePin
movePin (y, x) pin = let (originalY, originalX) = startIndex pin
                     in pin { startIndex = (originalY + y, originalX + x) }

showMultiplePins:: [Pin] -> String
showMultiplePins [] = ""
showMultiplePins (p : ps) = show p ++ showMultiplePins ps

-- | Change the Appearence of a pin
changeAppearence:: Grid -> UpdatePin
changeAppearence newGrid pin = pin { grid = newGrid
                                   , hasChanged = True } 
