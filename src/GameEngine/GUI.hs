{-# LANGUAGE RankNTypes #-}

module GameEngine.GUI where

import Control.Monad.State
   ( MonadIO(liftIO), MonadState(get, put), StateT(runStateT) )

import GameEngine.Pinboard (UpdatePinboard )
import GameEngine.Pin ( Pin(..), UpdatePin )
import GameEngine.PinboardUtilities
    ( alterPinboard, alterPinboardRoot, printPinboard, clearPinboard )
import GameEngine.PinUtilities (printPinWithColor,  alterPin ) 
import GameEngine.Playground (Playground )
import GameEngine.Picture ( Picture )


type Identifier = Int

{-|
    The 'GUI' arranges a root 'Pin', one 'Playground' and multiple 'Picture's.
    The entirety then illustrates the game.
-}
data GUI = GUI {
    root :: Pin,
    playground :: Playground,
    windows :: [Picture]
}

instance Show GUI where
    show gui = "<Root>" ++ (show $ root gui) ++ "</Root>\n"
            ++ "<Playground>" ++ (show $ playground gui) ++ "</Playground>\n"
            ++ concat (map (clasp.show) $ windows gui)

-- | Puts a string between "window"-brackets
clasp:: String -- ^ The expression to put in between "window"-brackets 
     -> String
clasp expression = "<Window>" ++ expression ++ "</Window>\n"

{-|
    Alter a GUI by first applying given functions onto the playground and then onto
    the windows.
-}
alterGUI:: UpdatePinboard -- ^ The function that shall modify the playground and the windows of the GUI
        -> StateT GUI IO GUI
alterGUI f = do gui <- get
                newPlayground <- liftIO $ runStateT (alterPinboard f)
                                        $ playground gui
                newWindows <- liftIO $ mapM (runStateT $ alterPinboard f) 
                                     $ windows gui
                put $ gui { playground = snd newPlayground,
                            windows = map snd newWindows }
                return gui

{-|
    Alter the roots of the GUI, of the playground and of the windows in that order.
-}
alterGUIRoots:: UpdatePin -- ^ The function used to modify the roots of the GUI
             -> StateT GUI IO GUI
alterGUIRoots f = do gui <- get
                     newRoot <- liftIO $ runStateT (alterPin f)
                                       $ root gui
                     newPlayground <- liftIO $ runStateT (alterPinboardRoot f)
                                             $ playground gui
                     newWindows <- liftIO $ mapM (runStateT $ alterPinboardRoot f) 
                                          $ windows gui
                     put $ gui { root = snd newRoot,
                                 playground = snd newPlayground,
                                 windows = map snd newWindows }
                     return gui

{-|
    Prints a GUI. If changes have been made to a window, which have caused
    a 'True'-status for the respective window, the status will be adjusted to 'False',
    since it is now printed on the terminal.
-}
printGUI:: StateT GUI IO GUI
printGUI = do gui <- get
              let offset = startIndex.root $ gui  -- read the offset for the windows induced by the GUI-root
              newRoot <- liftIO $ runStateT (printPinWithColor offset) $ root gui
              newPlayground <- liftIO $ runStateT (printPinboard offset) 
                                      $ playground gui
              newWindows <- liftIO $ mapM (runStateT $ printPinboard offset) 
                                   $ windows gui
              let newGui = gui { root = snd newRoot,
                                 playground = snd newPlayground,
                                 windows = map snd newWindows }
              put newGui
              return gui

{-|
    Erases non-existing pins from each pinboard. Assumes the same amount of pinboards
    for the old and the new gui.
-}
clearGUI:: GUI -- ^ The old GUI-state
        -> StateT GUI IO GUI
clearGUI oldGui = do gui <- get
                     liftIO $ clearPinboard ( (playground oldGui)
                                            , (playground gui) )
                     liftIO $ mapM clearPinboard 
                            $ zip (windows oldGui) (windows gui)
                     return gui 
