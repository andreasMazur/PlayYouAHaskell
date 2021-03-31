module GameEngine.Picture where

import qualified GameEngine.Pinboard as Pb ( Pinboard(..) )
import GameEngine.Pin ( Pin )
import GameEngine.PinUtilities (alterPin', showMultiplePins,  setStatus )
import {-# SOURCE #-} GameEngine.Call ( Call )
import Control.Monad.State

data Picture = Picture {
    pinboard :: Pin,
    pins :: [Pin],
    identifier :: Int,
    calls :: [Call],
    description :: String
}

instance Show Picture where
    show pic = "<Picture>\n"
            ++ "<Description>" ++ (show $ description pic) ++ "</Description>\n"
            ++ (show $ pinboard pic)
            ++ (showMultiplePins $ pins pic)
            ++ "<Identifier>" ++ (show $ identifier pic) ++ "</Identifier>\n"
            ++ "<Calls>" ++ (show $ calls pic) ++ "</Calls>\n"
            ++ "</Picture>\n"

instance Eq Picture where
    p1 == p2 = (pinboard p1) == (pinboard p2)
                && (pins p1) == (pins p2)
                && (identifier p1) == (identifier p2)

exchangeId:: Int -> Picture -> Picture
exchangeId id picture = picture { identifier = id }

setCalls:: [Call] -> Picture -> Picture
setCalls cs picture = picture { calls = cs }

exchangePinboard:: Pin -> Picture -> Picture
exchangePinboard pin picture = picture { pinboard = pin }

exchangePins:: [Pin] -> Picture -> Picture
exchangePins pins picture = picture { pins = pins }

setStatusPi:: Bool -> Picture -> Picture
setStatusPi status picture = picture {
    pinboard = execState (alterPin' $ setStatus status) $ pinboard picture,
    pins = map (execState (alterPin' $ setStatus status)) $ pins picture
}

instance Pb.Pinboard Picture where
    background = pinboard
    exchangeBackground = exchangePinboard
    pins = pins
    exchangePins = exchangePins
    id_pb = identifier
    exchange_id_pb = exchangeId
    setStatus = setStatusPi
    setCalls = setCalls
    getCalls = calls
    description_pb = description
