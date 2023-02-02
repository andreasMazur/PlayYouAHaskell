{-# LANGUAGE RankNTypes #-}

module GameEngine.PinboardUtilities where

import Control.Monad.State (execState, runStateT, put, liftIO, get,  StateT )
import Data.Array.IO (MArray(getBounds))

import GameEngine.PinUtilities (changeAppearence, alterPin',
    forcePrintPin,
    alterPin,
    printPin
 )
import GameEngine.Pinboard (Pinboard(..), UpdatePinboard)
import GameEngine.Pin (Pin(..), UpdatePin)
import GameEngine.Grid (writeMGrids,
    readGrid,
    createGrid,
    Grid,
    displayGrid
 )
import GameEngine.Terminal ( resetCursor )
import {-# SOURCE #-} GameEngine.Call ( Call )
import Data.List ((\\))
import Physics.Collision (collision)
import Physics.AABB (createAABBForPin)
import {-# SOURCE #-} GameEngine.Call (setInitiator)

type Identifier = Int
type Offset = (Int, Int)
type StartIndex = (Int, Int)

-- | Alter a pinboard.
alterPinboard:: Pinboard a => UpdatePinboard -- ^ The function used to modify the pinboard
                           -> StateT a IO a
alterPinboard f = do pinboardInitial <- get
                     pinboardNew <- liftIO $ f pinboardInitial
                     put pinboardNew
                     return pinboardInitial

-- | Applies a function onto the pinboard-root
alterPinboardRoot:: Pinboard a => UpdatePin -- ^ The function used to modify the pinboard-root
                               -> StateT a IO a
alterPinboardRoot f = do pb <- get
                         (_, newPb) <- liftIO $ runStateT (alterPin f)
                                              $ background pb
                         put $ exchangeBackground newPb pb
                         return pb

-- | Print a pinboard. (Lazily!)
printPinboard:: Pinboard a => Offset -- ^ An offset given by the root-window's start-index
                           -> StateT a IO a
printPinboard prevOffset@(offsetGuiY, offsetGuiX)
    = do pb <- get
         let (offsetPbY, offsetPbX) = startIndex.background $ pb
             offsetSum = (offsetGuiY + offsetPbY, offsetGuiX + offsetPbX)
             foregroundPinsLast =  [pin | pin <- pins pb, inBackground pin]
                                ++ [pin | pin <- pins pb, not.inBackground $ pin]
         newBg <- liftIO $ runStateT (printPin prevOffset)
                         $ background pb
         newPs <- liftIO
                $ mapM (runStateT $ printPin offsetSum) foregroundPinsLast
         let newPb = exchangePins (map snd newPs)
                   $ exchangeBackground (snd newBg) pb
         liftIO resetCursor
         put newPb
         return pb

-- | Erases all pins from the terminal which have been changed.
clearPinboard:: Pinboard a => (a, a) -- ^ (The old pinboard, The new pinboard)
                           -> IO ()
clearPinboard (oldPb, newPb)
    = do bg <- newBground
         eraser <- mapM (createEraser bg) $ zip changedPins startIndicesChangedPins
         let startIndices = map computeStartIndex startIndicesChangedPins
         mapM_ (uncurry displayGrid) $ zip eraser startIndices
         resetCursor
    where
        -- pin-ids which have changed in the new pinboard
        changedPinIds:: [Identifier]
        changedPinIds = [id_pin pin | pin <- pins newPb
                                    , hasChanged pin]

        -- old pins, which have changed in the new pinboard
        changedPins:: [Pin]
        changedPins = [ pin | pin <- pins oldPb
                      , id_pin pin `elem` changedPinIds] ++ missingPins

        -- old pins, which do not exist anymore in the new pinboard
        missingPins:: [Pin]
        missingPins = pins oldPb \\ pins newPb

        startIndicesChangedPins:: [StartIndex]
        startIndicesChangedPins = map startIndex changedPins

        computeStartIndex:: StartIndex -> StartIndex
        computeStartIndex (y, x)
            = let (yOffset, xOffset) = startIndex.background $ newPb
              in (y + yOffset, x + xOffset)

        -- creates erasers for changed or missing pins
        createEraser:: Grid -> (Pin, StartIndex) -> IO Grid
        createEraser backGround (pin, (y, x))
            = do (_, (yBound, xBound)) <- getBounds $ grid pin
                 grid <- createGrid (yBound, xBound)
                                    [' ' | _ <- [1..(yBound*xBound)]]
                 readGrid backGround grid (y+1, x+1)

        -- add background pins to the background, if they intersect with
        -- another pin
        newBground:: IO Grid
        newBground = do let (grids, startIndices) = unzip consideredBgPins
                        writeMGrids bground grids startIndices

        bground:: Grid
        bground = grid.background $ oldPb

        consideredBgPins:: [(Grid, StartIndex)]
        consideredBgPins = [ (grid pin, incrementTuple $ startIndex pin)
                           | pin <- pins oldPb
                           , inBackground pin
                           , intersects pin ]
            where
                incrementTuple:: (Int, Int) -> (Int, Int)
                incrementTuple (y, x) = (y+1, x+1)

        -- Checks if a pin intersects with any pin from oldPb
        intersects:: Pin -> Bool
        intersects pin
            = any (collision (createAABBForPin pin) . createAABBForPin)
                  (pins oldPb)

-- | A function to modify the 'Pin's of a pinboard.
accessPins:: UpdatePin -- ^ The function that shall update the pins
          -> UpdatePinboard
accessPins f pb = do alteredPins <- mapM (runStateT $ alterPin f) (pins pb)
                     let newPins = map snd alteredPins
                     return $ exchangePins newPins pb

-- | A function to modify a specific 'Pin'.
alterSpecificPin:: UpdatePin -- ^ The function to apply onto the target 'Pin'
                -> Identifier -- ^ The ID of the target 'Pin'
                -> UpdatePinboard
alterSpecificPin f id pb
    = do let alteredPins = [alterP p | p <- pins pb]
         return $ exchangePins alteredPins pb
    where
        alterP:: UpdatePin
        alterP pin
            | id_pin pin == id = execState (alterPin' f) pin
            | otherwise = pin

-- | A function to add a 'Pin' to a 'Pinboard'
addPin:: Pin -- ^ The 'Pin' that shall be added
      -> UpdatePinboard
addPin p pb = let newPins = p : pins pb
              in return $ exchangePins newPins pb

-- | A function to add multiple pins to a 'Pinboard'
addPins:: [Pin] -- ^ The 'Pin's to add
       -> UpdatePinboard
addPins [] pb = return pb
addPins (p:ps) pb = do newPb <- addPin p pb
                       addPins ps newPb

-- | A function to add a 'Pin' to a 'Pinboard' (without IO monad)
addPin':: Pinboard a => Pin -- ^ The 'Pin' that shall be added
                     -> a -- ^ The 'Pinboard' in which the 'Pin' shall be added
                     -> a
addPin' p pb = let newPins = p : pins pb
               in exchangePins newPins pb

-- | A function to add multiple pins to a 'Pinboard' (without IO monad)
addPins':: Pinboard a => [Pin] -- ^ The 'Pin's to add
                      -> a -- ^ The 'Pinboard' in which the 'Pin's shall be added
                      -> a
addPins' ps pb = foldl (flip addPin') pb ps

-- | A function to remove a 'Pin' from a 'Pinboard'
removePins:: [Identifier] -- ^ The ID of the 'Pin' that shall be removed
          -> UpdatePinboard
removePins ids pb = let newPins = [pin | pin <- pins pb
                                       , id_pin pin `notElem` ids]
                     in return $ exchangePins newPins pb

-- | A function to remove all 'Pin's from a given 'Pinboard'
removeAllPins:: UpdatePinboard
removeAllPins pb = return $ exchangePins [] $ setStatus True pb

-- | Returns the 'Pin's with a specific ID of a given 'Pinboard'
getPins:: Pinboard a => Identifier -- ^ The ID of the target 'Pin's
                     -> a -- ^ The 'Pinboard' from which to fetch the 'Pin's
                     -> [Pin]
getPins id pb = [pin | pin <- pins pb, id_pin pin == id]

-- | A function to reset the terminal
forcePrintPinboard:: UpdatePinboard
forcePrintPinboard = accessPins forcePrintPin

-- | A function to add calls to existing 'Call's
addCalls:: Pinboard a => [Call] -- ^ The 'Call's that shall be added
                      -> a -- ^ The 'Pinboard' to which the 'Call's shall be added
                      -> a
addCalls calls pb
    = setCalls (getCalls pb ++ map (setInitiator $ id_pb pb) calls) pb

-- | Print a pinboard without waiting for the game loop
printPinboardNoLoop:: Pinboard a => a -- ^ The 'Pinboard' to print
                                 -> a -- ^ The old state of the 'Pinboard' to print
                                 -> IO a
printPinboardNoLoop newPb pb
    = do clearPinboard (pb, newPb)
         (_, newPb2) <- runStateT (printPinboard (0, 0)) newPb
         return newPb2

-- | Changes the appearence of a 'Pin'
changeAppearenceOfPin:: Identifier -- ^ The ID of the target 'Pin'
                     -> String -- ^ The new look of the 'Pin'
                     -> (Int, Int) -- ^ The dimension of the target 'Pin'
                     -> UpdatePinboard
changeAppearenceOfPin pinId newAppearence dim pb
    = do newGrid <- createGrid dim newAppearence
         alterSpecificPin (changeAppearence newGrid) pinId pb
