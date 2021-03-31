
module GameConfig.UserInterface where

import GameEngine.Picture ( Picture(..) )
import GameEngine.Pin ( Pin )
import GameEngine.PinUtilities (createPin' )

type Dimension = (Int, Int)
type Identifier = Int


-- ## PLAYGROUND SETTINGS ##

-- | The start index for the playground
playgroundStartIndex:: (Int, Int)
playgroundStartIndex = (1, 1)

-- | Identifier for the playground
playgroundID:: Identifier
playgroundID = 3

-- | The ground height in the playground
ground :: Int
ground = fst playgroundDim - 1

-- | Dimension for the playground
playgroundDim:: Dimension
playgroundDim = (20, 80)

-- ## BACKGROUND SETTINGS ##

-- | Backgound of the game
rootOffset:: (Int, Int)
rootOffset = (0, 0)

rootPin:: IO Pin
rootPin = createPin' root rootDim rootOffset 0 []

rootDim:: Dimension
rootDim = (30, 110)

root:: String  -- 30x110 ASCII-symbols
root = "==============================================================================================================\
       \|                                                                                 ^                          |\
       \|                                                                                 ^    Play You a Haskell    |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^<< << << << <> >> >> >> >>|\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^<< << << << <> >> >> >> >>|\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^                          |\
       \|                                                                                 ^<< << << << <> >> >> >> >>|\
       \|                                                                                 ^           \\              |\
       \|                                                                                 ^           /\\             |\
       \=============================================================================================================="

-- ## MULTI-USE-BOX SETTINGS ##

-- | Multi use box
multiUseBox_P:: IO Picture
multiUseBox_P = do pb <- createPin' multiUseBox multiUseBoxDim multiUseBoxStartIndex 0 []
                   return $ Picture {
                        pinboard = pb,
                        pins = [],
                        identifier = multiUseBoxID,
                        calls = [],
                        description = multiUseBoxDesc
                   }

multiUseBoxDesc:: String
multiUseBoxDesc = "Multi use box"

-- | Start index for the multi-use-box
multiUseBoxStartIndex:: (Int, Int)
multiUseBoxStartIndex = (14, 83)

-- | Identifier for the multi-use-box
multiUseBoxID:: Identifier
multiUseBoxID = 2

-- | Dimension for the multi-use-box
multiUseBoxDim:: Dimension
multiUseBoxDim = (12, 26)

multiUseBox:: String -- 12x26 ASCII-symbols
multiUseBox = "                          \
              \                          \
              \                          \
              \                          \
              \                          \ 
              \                          \
              \                          \
              \                          \
              \                          \
              \                          \
              \                          \
              \                          "

-- ## INTERACTION BOX SETTINGS ##

-- Interaction box
interBox_P:: IO Picture
interBox_P = do pb <- createPin' interBox interBoxDim interBoxStartIndex 0 []
                return $ Picture {
                    pinboard = pb,
                    pins = [],
                    identifier = interBoxID,
                    calls = [],
                    description = interBoxDesc
                }

interBoxDesc:: String
interBoxDesc = "Interaction box"

-- | Start index for the interaction box
interBoxStartIndex:: (Int, Int)
interBoxStartIndex = (5, 83)

-- | Identifier for the interaction box
interBoxID:: Identifier
interBoxID = 1

-- | Dimension for the interaction box
interBoxDim:: Dimension
interBoxDim = (8, 26)

interBox:: String -- 8x26 ASCII-symbols
interBox =  "                          \
            \                          \
            \                          \
            \                          \
            \                          \ 
            \                          \
            \                          \
            \                          "

-- ## TEXTBOX SETTINGS ##

textBox_P:: IO Picture
textBox_P = do pb <- createPin' textBox textBoxDim textBoxStartIndex 0 []
               return $ Picture {
                    pinboard = pb,
                    pins = [],
                    identifier = textBoxID,
                    calls = [],
                    description = textBoxDesc
               }

textBoxDesc:: String
textBoxDesc = "Textbox"

-- | Start index for the textbox
textBoxStartIndex:: (Int, Int)
textBoxStartIndex = (22, 1)

-- | Identifier for the textbox
textBoxID:: Identifier
textBoxID = 0

-- | Dimension for the textbox
textBoxDim:: Dimension
textBoxDim = (7, 81)

textBox:: String  -- 7x81 ASCII-symbols
textBox = "                                                                                 \
          \                                                                                 \
          \                                                                                 \
          \                                                                                 \
          \                                                                                 \
          \                                                                   ______________\
          \                                                                   |Eingabe:     "

-- ## CONTROLS PICTURE ##

controlsMenu_P:: IO Picture
controlsMenu_P = do pb <- createPin' controlsMenu controlsMenuDim controlsMenuStartIndex 0 []
                    return $ Picture {
                        pinboard = pb,
                        pins = [],
                        identifier = controlsMenuID,
                        calls = [],
                        description = controlsMenusDesc
                    }

controlsMenusDesc:: String
controlsMenusDesc = "Control menu"

controlsMenuStartIndex:: (Int, Int)
controlsMenuStartIndex = (1, 110)

controlsMenuID:: Identifier
controlsMenuID = 4

controlsMenuDim:: Dimension
controlsMenuDim = (16, 38)

controlsMenu:: String  -- 16x37 ASCII-symbols
controlsMenu = "       ***** Steuerung *****          \
               \                                      \
               \  - Nach links gehen  : 'a'           \
               \  - Nach rechts gehen : 'd'           \
               \                                      \
               \  - Hochspringen         : 'w'        \
               \  - Nach links springen  : 'q'        \
               \  - Nach rechts springen : 'e'        \
               \                                      \
               \  - Inventar/Menu verlassen : 'l'     \
               \  - Menu betreten           : 'm'     \
               \  - Inventar betreten       : 'i'     \
               \  - Aktion auswählen        : '\\n'    \
               \                                      \
               \  - Anzeigefehler beheben : 'r'       \
               \  - Spiel beenden         : 'p'       "

-- ## DEBUG WINDOW ##

debugWindow_P:: IO Picture
debugWindow_P = do pb <- createPin' debugWindow debugWindowDim debugWindowStartIndex 0 []
                   return $ Picture {
                       pinboard = pb,
                       pins = [],
                       identifier = debugWindowID,
                       calls = [],
                       description = debugWindowDesc
                   }

debugWindowDesc:: String
debugWindowDesc = "Debug window"

debugWindowStartIndex:: (Int, Int)
debugWindowStartIndex = (30, 1)

debugWindowID:: Identifier
debugWindowID = 5

debugWindowDim:: Dimension
debugWindowDim = (7, 81)

debugWindow:: String  --7x81 ASCII-symbols
debugWindow = "                                                                                 \
              \                                                                                 \
              \                                                                                 \
              \                                                                                 \
              \                                                                                 \
              \                                                                                 \
              \                                                                                 "
