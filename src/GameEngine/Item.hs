
module GameEngine.Item where

import Data.Typeable ( Typeable )

import GameEngine.Call (identityCall, Call(..) )

type StartIndex = (Int, Int)

{-|
    An item can be carried by any object (including but not limited to the player). It can
    carry 'Call's which can be executed when using the item.
-}
data Item = Item {
    description_item :: String,
    belongsTo :: String,
    displayName :: String,
    interaction_item :: Call,
    display_item :: Bool
} deriving (Typeable)

instance Eq Item where
    i1 == i2 = description_item i1 == description_item i2

instance Show Item where
    show item = description_item item

-- | A placeholder item.
placeHolderItem:: Item
placeHolderItem = Item {
    description_item = "",
    belongsTo = "",
    displayName = "",
    interaction_item = identityCall,
    display_item = False
}

-- ## SET FUNCTIONS ##

removeVisibility:: Item -> Item
removeVisibility item = item { interaction_item = identityCall
                             , display_item = False }
