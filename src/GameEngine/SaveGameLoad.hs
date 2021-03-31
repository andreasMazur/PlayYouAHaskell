
module GameEngine.SaveGameLoad where

import GameEngine.Call (setNested, executeCalls, Call(..))
import GameConfig.UserInterface (debugWindowID, controlsMenuID, textBoxID, interBoxID, multiUseBoxID, playgroundID)
import GameEngine.Pinboard (UpdatePinboard)
import GameEngine.StandardCalls (putTextCall2, changePic, changePg_noP)
import GameEngine.Picture (Picture)

import GameEngine.SaveGameParser ( parseSaveGameFile )

type Identifier = Int

loadGame:: Call
loadGame = Call {
    id_c = -3141,
    description = "Spiel laden",
    initiator = -1,
    receiver = playgroundID,
    task = gameLoader,
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

gameLoader:: UpdatePinboard
gameLoader pb 
    = do (newPg, newMultiUseBox, newInterBox, newTextBox, newControlsMenu, newDebugWindow ) <- parseSaveGameFile
         executeCalls [ setNested True $ putTextCall2 1 "" loadsofText ""
                      , setNested True $ changePg_noP "" newPg ""
                      , loadWindow multiUseBoxID newMultiUseBox
                      , loadWindow interBoxID newInterBox
                      , loadWindow textBoxID newTextBox
                      , loadWindow controlsMenuID newControlsMenu
                      , loadWindow debugWindowID newDebugWindow ] pb
    where
        loadsofText = "Lädt.. [ Kann einen Augenblick dauern, nicht wild\
                      \ rumtippen;) ]"

loadWindow:: Identifier -> IO Picture -> Call
loadWindow ide newPic = setNested True $ changePic "" newPic ide ""
