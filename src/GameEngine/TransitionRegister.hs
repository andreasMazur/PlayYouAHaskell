
module GameEngine.TransitionRegister where

import {-# SOURCE #-} GameEngine.Playground (createPlayground, Playground)

identityPg:: IO Playground
identityPg = createPlayground "" "identity playground" [] []

transitionRegister:: [IO Playground]
transitionRegister = []
