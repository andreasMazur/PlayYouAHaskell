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

data GUI = GUI {
    root :: Pin,
    playground :: Playground,
    windows :: [Picture]
}

instance Show GUI where
    show gui = "<Root>" ++ (show $ root gui) ++ "</Root>\n"
            ++ "<Playground>" ++ (show $ playground gui) ++ "</Playground>\n"
            ++ concat (map (clasp.show) $ windows gui)

clasp:: String -> String
clasp expression = "<Window>" ++ expression ++ "</Window>\n"

-- | Alter a GUI.
-- | 'UpdatePinboard' : The function that shall modify windows of the GUI.
alterGUI:: UpdatePinboard -> StateT GUI IO GUI
alterGUI f = do gui <- get
                newPlayground <- liftIO $ runStateT (alterPinboard f)
                                        $ playground gui
                newWindows <- liftIO $ mapM (runStateT $ alterPinboard f) 
                                     $ windows gui
                put $ gui { playground = snd newPlayground,
                            windows = map snd newWindows }
                return gui

-- | Alter the roots of a GUI
-- | 'UpdatePin' : The function used to modify the roots of the GUI
alterGUIRoots:: UpdatePin -> StateT GUI IO GUI
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

-- | Prints a GUI. If changes have been made to a sub-window, which have caused
--   a 'True'-status for a sub-window, the status will be adjusted to 'False',
--   since they are now printed on the terminal.
printGUI:: StateT GUI IO GUI
printGUI = do gui <- get
              let offset = startIndex.root $ gui  -- read the offset for the sub-windows induced by the root-window.
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

-- | Erases non (anymore) existing pins from each pinboard
--   Assumes the same amount of pinboards for the old and the new gui.
clearGUI:: GUI -> StateT GUI IO GUI
clearGUI oldGui = do gui <- get
                     liftIO $ clearPinboard ( (playground oldGui)
                                            , (playground gui) )
                     liftIO $ mapM clearPinboard 
                            $ zip (windows oldGui) (windows gui)
                     return gui 
