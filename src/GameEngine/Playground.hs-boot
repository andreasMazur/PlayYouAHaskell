
module GameEngine.Playground where

import GameEngine.Object ( UpdateObject, Object )
import GameEngine.Pinboard (Pinboard)
import {-# SOURCE #-} GameEngine.Call (Call)
import {-# SOURCE #-} GameEngine.Item (Item)

data Playground
instance Eq Playground
instance Pinboard Playground

type UpdatePlayground = Playground -> Playground
type Identifier = Int
type Dimension = (Int, Int)
type StartIndex = (Int, Int)

applyOnObject:: Identifier -> UpdateObject -> UpdatePlayground
addObject:: Object -> IO Playground -> IO Playground
createPlayground:: String
                -> String 
                -> [Call]
                -> [(String, Dimension, StartIndex, [Call], Bool, Bool, [Item])]
                -> IO Playground
