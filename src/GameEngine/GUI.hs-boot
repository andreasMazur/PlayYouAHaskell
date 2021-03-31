
module GameEngine.GUI where

import GameEngine.Pin ( Pin )
import GameEngine.Playground ( Playground )
import GameEngine.Picture ( Picture )

data GUI = GUI {
    root :: Pin,
    playground :: Playground,
    windows :: [Picture]
}