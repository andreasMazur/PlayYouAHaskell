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

-- | Create a 'Pin'
createPin:: String -- ^ The appearence of the 'Pin'
         -> Dimension -- ^ The size of the 'Pin'
         -> StartIndex -- ^ The start-index of the 'Pin'
         -> Identifier -- ^ The ID of the 'Pin'
         -> [Call] -- ^ The 'Call's that the 'Pin' shall contain
         -> Bool -- ^ Whether the 'Pin' is rigid
         -> Bool -- ^ Whether the 'Pin' is located in the background
         -> [Item] -- ^ The 'Item's the 'Pin' shall hold
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

-- | Creates a default 'Pin' that is rigid, not in the background without any 'Item's
createPin':: String -- ^ The appearence of the 'Pin'
          -> Dimension -- ^ The size of the 'Pin'
          -> StartIndex -- ^ The start-index of the 'Pin'
          -> Identifier -- ^ The ID of the 'Pin'
          -> [Call] -- ^ The 'Call's that the 'Pin' shall contain
          -> IO Pin
createPin' appearence dimension startIndex id calls
  = createPin appearence dimension startIndex id calls True False []

-- | Creates a 'Pin' with a 'Grid' instead of a 'String'
createPinWithGrid:: (IO Grid) -- ^ The appearence of the 'Pin'
                 -> StartIndex -- ^ The size of the 'Pin'
                 -> Identifier -- ^ The ID of the 'Pin'
                 -> [Call] -- ^ The 'Call's that the 'Pin' shall contain
                 -> [Item] -- ^ The 'Item's the 'Pin' shall hold
                 -> Bool -- ^ Whether the 'Pin' is rigid
                 -> Bool -- ^ Whether the 'Pin' is located in the background
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

-- | Alters a pin. (uses 'State'-transformer monad with IO)
alterPin:: UpdatePin -- ^ The function used to modify the 'Pin'
        -> StateT Pin IO Pin
alterPin f = do initialPin <- get
                modify f
                pin <- get
                if initialPin == pin
                  then return initialPin
                  else do let newPin = pin { hasChanged = True }
                          put newPin  -- store the new pin
                          return initialPin

-- | Alters a pin. (uses simple 'State' monad)
alterPin':: UpdatePin -- ^ The function used to modify the 'Pin'
         -> State Pin Pin
alterPin' f = do initialPin <- get
                 modify f
                 pin <- get
                 if initialPin == pin
                   then return initialPin
                   else do let newPin = pin { hasChanged = True }
                           put newPin  -- store the new pin
                           return initialPin

-- | Prints a pin.
printPin:: Offset -- ^ An offset given by the 'Pin's 'Pinboard'
        -> StateT Pin IO Pin
printPin (yOffset, xOffset)
    = do pin <- get
         if hasChanged pin
           then do let (y, x) = startIndex pin
                   liftIO $ displayGrid (grid pin) (y + yOffset, x + xOffset)
                   put pin { hasChanged = False }
                   return pin
           else return pin

-- | TODO (Shall print a singular 'Pin' in a given color)
printPinWithColor:: Offset 
                 -> StateT Pin IO Pin
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

-- | Forces to print the given 'Pin'
forcePrintPin:: UpdatePin
forcePrintPin pin = pin { hasChanged = True }

-- | Move the pin for an offset
movePin:: Offset -- ^ The offset (y, x) by which the 'Pin' shall be moved
       -> UpdatePin
movePin (y, x) pin = let (originalY, originalX) = startIndex pin
                     in pin { startIndex = (originalY + y, originalX + x) }

-- | Converts a list of 'Pin's into 'String's
showMultiplePins:: [Pin] -- ^ The 'Pin's to convert
                -> String
showMultiplePins [] = ""
showMultiplePins (p : ps) = show p ++ showMultiplePins ps

-- | Change the Appearence of a pin
changeAppearence:: Grid -- ^ The new appearence
                -> UpdatePin
changeAppearence newGrid pin = pin { grid = newGrid
                                   , hasChanged = True } 
