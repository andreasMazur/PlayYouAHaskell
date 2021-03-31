
module GameConfig.PlayerModel (
    playerModel_Dim,
    playerModel_ID,
    playerModel
) where

type Identifier = Int
type Dimension = (Int, Int)

playerModel_ID:: Identifier
playerModel_ID = 0

playerModel_Dim:: Dimension
playerModel_Dim = (5, 3)

playerModel:: String  --5x3 ASCII-symbols
playerModel = " _ \
              \( )\
              \/|\\\
              \ | \
              \/ \\"
