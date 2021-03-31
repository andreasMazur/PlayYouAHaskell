
module GameEngine.Call where

data Call
instance Show Call
instance Eq Call

setPersistence:: Bool -> Call -> Call
isDisposable:: Call -> [Call] -> Bool
setInitiator:: Int -> Call -> Call
