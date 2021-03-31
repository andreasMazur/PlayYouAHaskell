{-# LANGUAGE RankNTypes #-}

module GameEngine.Pinboard where

class Pinboard a

type UpdatePinboard = forall a. Pinboard a => a -> IO a