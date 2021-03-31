
module GameEngine.Pin where

data Pin
startIndex:: Pin -> (Int, Int)
id_pin:: Pin -> Int

type UpdatePin = Pin -> Pin
