module Game.Aff.Web.Events where


import Prelude

import Control.Monad.Error.Class (throwError)
import Data.List (List, fromFoldable, (:))
import Data.List.Partial (head)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable, traverse, sequence)
import Effect (Effect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Game
import Game.Util
import Game.Aff.Web
import Partial.Unsafe (unsafePartial)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement (fromElement, toEventTarget, value, valueAsNumber) as Input


-- All functions take `EventTarget`s
-- Function that gets an EventTarget from a `QuerySelector`
-- Binding for document as `EventTarget` (`documentEventTarget`)
-- Function to make event updates (`eventUpdate`)
-- Function that allows for easy mangaging of doing something when one of many events fires
  -- don't think this would work for all event types. maybe.
  -- could be possible if each event had a function that can get the values without the event firing
    -- at least some events could have this (`change`)
-- Funtion that allows an event to be an activator for some other function that has access to some data
  -- (do something with data from inputs when a button is pressed)
-- Preset events:
  -- mouse move (read: event object, mouse position relative to target)
  -- mouse click (read: event object, mouse position relative to target, mouse button)
  -- keydown (read: event object, key; match: key)
  -- keyup (read: event object, key; match: key)
  -- keypressed (read: event object, key; match: key)
  -- onload (read: event object)
  -- change (read: event object, value, valueAsNumber)