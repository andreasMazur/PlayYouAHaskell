
module GameEngine.TransitionRegister where

import {-# SOURCE #-} GameEngine.Playground (createPlayground, Playground)

-- | An empty 'Playground' which can be used as a placeholder
identityPg:: IO Playground
identityPg = createPlayground "" "identity playground" [] []

{-|
    The 'transitionRegister' is used to continue from an old chapter into a new one.
    Therefore, the i-th element in 'transitionRegister' contains the first 'Playground'
    in the (i+1)-th chapter. Also, see 'chapterTransitionCall'.
-}
transitionRegister:: [IO Playground]
transitionRegister = []
