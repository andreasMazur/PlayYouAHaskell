{-# LANGUAGE RankNTypes #-}

module GameEngine.Menu where

import GameEngine.Pinboard ( UpdatePinboard )
import {-# SOURCE #-} GameEngine.Call ( Call )

enterMenu:: Int -> UpdatePinboard
addNewIBoxCalls:: [Call] -> UpdatePinboard
updateIBoxCalls:: [Call] -> [Call] -> UpdatePinboard
