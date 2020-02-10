module Control.Game.Web where

import Prelude

import Control.Game
import Web.Event.Event
import Web.Event.EventTarget

-- TODO: implement AnimGame using a custom `requestAnimationFrame` that runs in
-- Aff.

-- will need a function like this:
-- requestAnimationFrames :: Effect a -> Aff Void
-- how can i make it resolve from that? ***untilJust***?
-- built from a function like this: Effect a -> Aff a (uses requestanimationframe)

-- function that takes a list of GameEvents and makes a (Ref s -> Aff a) from it
type GameEvent s a =
  { eventType  :: EventType
  , target     :: EventTarget
  , update     :: Event -> GameUpdate s a
  , useCapture :: Boolean
  }