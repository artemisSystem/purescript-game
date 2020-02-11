module Control.Game.Web where

import Prelude

import Control.Game
import Web.Event.Event
import Web.Event.EventTarget

-- TODO: implement AnimGame using a custom `requestAnimationFrame` that runs in
-- Aff. `requestAnimationFrame` functions shoule be built up similarly to
-- `after`, `every`, and `everyUntil`. Also with a newtype with ToUpdate. There
-- will be `requestAnimationFrame` and `requestAnimationFrame'`, the latter
-- with a `Window` argument.

-- function that takes a list of GameEvents and makes a (Ref s -> Aff a) from it
type GameEvent s a =
  { eventType  :: EventType
  , target     :: EventTarget
  , update     :: Event -> EffectUpdate s a
  , useCapture :: Boolean
  }