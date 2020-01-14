module Control.Game.EventHelpers where

import Prelude

import Control.Game (GameEvent)
import Control.Monad.Error.Class (throwError)
import Data.Maybe
import Effect (Effect)
import Effect.Exception (Error, error)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector, querySelector)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (EventTarget)
import Web.HTML (window)
import Web.HTML.HTMLCanvasElement (fromElement, HTMLCanvasElement)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (cancelAnimationFrame, requestAnimationFrame, document)

-- Helper functions for easily implementing events for buttons, getting values
-- from inputs when they change, or when a button is clicked. Including variants
-- for multiple inputs with ways to combine them, and options for whether to use
-- valueAsNumber or just value. Also mouse clicks, movements and keyboard inputs

-- TODO:
-- mouse clicks (event -> kleisli effect)
-- mouse clicks (position -> kleisli effect)
-- mouse clicks (position within some element -> kleisli effect)
-- mouse movements (event -> kleisli effect)
-- mouse movements (position -> kleisli effect)
-- mouse movements (position within some element -> kleisli effect)
-- keyboard inputs (keys -> kleisli effects)
-- keyboard inputs (event -> kleisli effect)
-- input tag (value, onchange)
-- input tag (value, on button click)
-- input tags (value, onchange)
-- input tags (value, on button click)
-- input tag (valueAsNumber, onchange)
-- input tag (valueAsNumber, on button click)
-- input tags (valueAsNumber, onchange)
-- input tags (valueAsNumber, on button click)

-- | Create a `GameEvent` that fires when the specified button is clicked
buttonEvent
  :: forall state
   . QuerySelector
  -> (Event -> state -> Effect state)
  -> Effect (GameEvent state)
buttonEvent button update = do
  doc <- window >>= document <#> toParentNode
  target <- querySelector button doc <#> map toEventTarget >>= case _ of
    Just elem -> pure elem
    Nothing -> throwError (error "Button for buttonEvent not found")
  pure
    { eventType: EventType "click"
    , target
    , update
    , useCapture: false
    }