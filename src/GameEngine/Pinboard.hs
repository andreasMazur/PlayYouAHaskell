{-# LANGUAGE RankNTypes #-}

module GameEngine.Pinboard where

import GameEngine.Pin ( Pin )
import {-# SOURCE #-} GameEngine.Call ( Call )

type Identifier = Int
type StartIndex = (Int, Int)
type UpdatePinboard = forall a. Pinboard a => a -> IO a

{-|
    PYaH's GUI contains multiple 'Pinboard's. Each 'Pinboard' can contain 'Pin's, which describe elements in the 
    environment. Additionally, 'Pinboard's communicate with other 'Pinboard's via 'Call's.
-}
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
