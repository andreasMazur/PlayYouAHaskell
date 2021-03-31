
module GameEngine.Save where

import GameEngine.Pinboard ( UpdatePinboard )
import GameEngine.Call ( executeCalls, Call(..) )
import GameConfig.UserInterface (
    debugWindowID,
    controlsMenuID,
    textBoxID,
    interBoxID,
    multiUseBoxID,
    playgroundID
 )

type Identifier = Int

saveGamePath:: FilePath
saveGamePath = "./app/saveGame"

saveGame:: Call
saveGame = Call {
    id_c = -314,
    description = "Spiel speichern",
    initiator = -1,
    receiver = playgroundID,
    task = saveGame',
    interaction = id,
    persistence = True,
    disposable = False,
    nestedCall = False,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = "",
    tripwire = False
}

saveGame':: UpdatePinboard
saveGame' pb = do writeFile saveGamePath ""
                  executeCalls [ savePinboard playgroundID
                               , savePinboard multiUseBoxID
                               , savePinboard interBoxID
                               , savePinboard textBoxID
                               , savePinboard controlsMenuID
                               , savePinboard debugWindowID ] pb

savePinboard:: Identifier -> Call
savePinboard re = Call {
    id_c = -3141,
    description = "save the game",
    initiator = -1,
    receiver = re,
    task = savePinboard',
    interaction = id,
    persistence = False,
    disposable = False,
    nestedCall = True,
    newPlayground = Nothing,
    newPicture = Nothing,
    executeImmediatly = False,
    callName = "",
    tripwire = False
}

savePinboard':: UpdatePinboard
savePinboard' pb = do appendFile saveGamePath $ show pb  -- write new save-game
                      return pb
