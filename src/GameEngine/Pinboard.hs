{-# LANGUAGE RankNTypes #-}

module GameEngine.Pinboard where

import GameEngine.Pin ( Pin )
import {-# SOURCE #-} GameEngine.Call ( Call )

type Identifier = Int
type StartIndex = (Int, Int)
type UpdatePinboard = forall a. Pinboard a => a -> IO a

-- | The pinboard is the foundation of the game.
--   Each pinboard contains 'Pin's which are desribed below.
--   Furthermore, the GUI contains multiple pinboards, and each
--   pinboard can communicate with the other pinboards.
class (Show a, Eq a) => Pinboard a where
    background :: a -> Pin
    exchangeBackground :: Pin -> a -> a
    pins :: a -> [Pin]
    exchangePins :: [Pin] -> a -> a
    setStatus :: Bool -> a -> a
    id_pb :: a -> Identifier
    exchange_id_pb :: Identifier -> a -> a
    setCalls :: [Call] -> a -> a
    getCalls :: a -> [Call]
    description_pb :: a -> String

