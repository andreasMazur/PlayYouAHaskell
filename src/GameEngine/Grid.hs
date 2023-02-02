
module GameEngine.Grid (
    Grid,
    createGrid,
    displayGrid,
    writeMGrids,
    readGrid,
    showGrid
) where

import Data.Array.IO
    ( IOArray,
      getAssocs,
      newListArray,
      readArray,
      writeArray,
      MArray(getBounds) )
import Data.List
    ( (\\) )
import System.Console.ANSI 
    ( setCursorColumn,
      setCursorPosition )
import GHC.IO 
    ( unsafePerformIO )


type Dimension = (Int, Int)
type StartIndex = (Int, Int)

{-|
    In general, PYaH uses grids to display information.
    A grid tells where to put a character on the screen.
    Which characters are shown lies in the hand of the
    designer.

    Grids are nothing more than mutable Arrays. Thus, we
    can modify information in-place when necessary.

    NOTE! The first Index of an element in the grid has the index (1,1)
-}
type Grid = IOArray Dimension Char

-- | Casts a grid into a string that can be stored in a save-game file
showGrid:: Grid -- ^ The grid to save
        -> String -- ^ The result string
showGrid grid =  "<GridStr>" ++ gridStr ++ "</GridStr>\n"
              ++ "<GridDim>" ++ (show gridDim) ++ "</GridDim>\n"
    where
        (gridDim, gridStr) = gridInfos grid

{-# NOINLINE gridInfos #-}
gridInfos:: Grid -> (Dimension, String)
gridInfos grid = (fst.last $ gridElems, map snd gridElems)
    where
        gridElems:: [(Dimension, Char)]
        gridElems = unsafePerformIO.getAssocs $ grid

fstIndexInGrid:: StartIndex
fstIndexInGrid = (1, 1)

-- | Create a new grid
createGrid:: Dimension -- ^ The dimension respectively the largest index in the grid
          -> String -- ^ The string, that shall be converted to a grid
          -> IO Grid -- ^ A Grid
createGrid dimension appearence 
    = newListArray (fstIndexInGrid, dimension) appearence

-- | Display a grid on the terminal
displayGrid:: Grid -- ^ The grid that shall be displayed
           -> StartIndex -- ^ The starting (row, column) for printing
           -> IO ()
displayGrid grid (rowOffset, colOffset) 
    = do elements <- getAssocs grid
         setCursorPosition rowOffset colOffset
         displayRectangle colOffset elements

-- | Display a rectangle on the terminal
displayRectangle:: Int -- ^ The starting column for printing
                -> [(StartIndex, Char)] -- ^ (Index, Element) of a grid
                -> IO ()
displayRectangle _ [] = return ()
displayRectangle offset rectangle@(((currentLine, _), _) : _)
    = do setCursorColumn offset
         putStrLn.(map snd) $ lineToShow
         displayRectangle offset $ rectangle \\ lineToShow
    where
        lineToShow = [((row, col), elem) | ((row, col), elem) <- rectangle
                                         , row == currentLine]

{- 
   When we paste into or read from a grid, there is always a second
   grid from which we paste or in which we store the read elements.
   Hence, we always have a background and a foreground grid. The
   background grid represents the grid in which we paste or from
   which we read. In order to paste/read properly, we have to know
   the indices in the background grid from which we read respectively
   paste into. These are computed with this function. 
-}
computeIndicesInBackgroundGrid:: Grid -- ^ The grid in which we store the read or paste from
                              -> StartIndex -- ^ The start index in the background grid from which the read respectively
                                            --   paste-into procedure starts.
                              -> IO [(Int, Int)] -- ^ Background grid indices
computeIndicesInBackgroundGrid  foregroundGrid startIndex
    = do boundsForegroundGrid <- getBounds foregroundGrid
         let heightInBackgroundGrid 
                = fst startIndex + (fst.snd $ boundsForegroundGrid) - 1
             widthInBackgroundGrid
                = snd startIndex + (snd.snd $ boundsForegroundGrid) - 1
             heightIndicesInBackgroundGrid
                = [(fst startIndex)..heightInBackgroundGrid]
             widthIndicesInBackgroundGrid
                = [(snd startIndex)..widthInBackgroundGrid]
         return $ [(x, y) | x <- heightIndicesInBackgroundGrid
                          , y <- widthIndicesInBackgroundGrid]

-- | Copy and pastes a grid onto another grid for given indices
copyPaste:: Grid -- ^ The grid in which copied values will be stored
         -> Grid -- ^ The grid from which shall be copied
         -> [((Int, Int), (Int, Int))] -- ^ The indices ((bgIndex, fgIndex) : xs)
         -> IO Grid -- ^ A (partially) copied grid
copyPaste toModify _ [] = return toModify
copyPaste toModify toPaste ((bgIndex, fgIndex) : xs)
     = do char <- readArray toPaste fgIndex
          writeArray toModify bgIndex char
          copyPaste toModify toPaste xs

-- | Write multiple grids on one grid
writeMGrids:: Grid -- ^ The grid in which we paste into
           -> [Grid] -- ^ The grids from which we paste into 'toModify'
           -> [(Int, Int)] -- ^ The start indices in 'toModify' from which we start pasting
           -> IO Grid -- ^ The result grid
writeMGrids toModify [] [] = return toModify
writeMGrids toModify (toPaste : xs) (startIndex : ys)
    = do newToModify <- writeGrid toModify toPaste startIndex
         writeMGrids newToModify xs ys

-- | Write a grid onto another grid
writeGrid:: Grid -- ^ The grid in which we paste into
         -> Grid -- ^ The grid from which we paste into 'toModify'
         -> (Int, Int) -- ^ The start index in 'toModify' from which we start pasting
         -> IO Grid -- ^ The result grid
writeGrid toModify toPaste startIndex
    = do bgIndices <- computeIndicesInBackgroundGrid toPaste startIndex
         boundsToPasteArray <- getBounds toPaste
         let fgIndices = [(x, y) | x <- [1..fst.snd $ boundsToPasteArray]
                                 , y <- [1..snd.snd $ boundsToPasteArray]]
             zipedIndices = zip bgIndices fgIndices
         copyPaste toModify toPaste zipedIndices

-- | Read from a grid
readGrid:: Grid -- ^ The grid from which we read
        -> Grid -- ^ The grid in which we store the read elements
        -> (Int, Int) -- ^ The start index in 'readFrom' from which we start reading
        -> IO Grid -- ^ The read grid
readGrid readFrom storeIn startIndex
    = do bgIndices <- computeIndicesInBackgroundGrid storeIn startIndex
         boundsToStoreInArray <- getBounds storeIn
         let fgIndices = [(x, y) | x <- [1..fst.snd $ boundsToStoreInArray]
                                 , y <- [1..snd.snd $ boundsToStoreInArray]]
             zipedIndices = zip fgIndices bgIndices
         copyPaste storeIn readFrom zipedIndices
