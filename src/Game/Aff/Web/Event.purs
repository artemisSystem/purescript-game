module Game.Aff.Web.Event
where {-}
  ( qSelEventTarget
  , windowEventTarget
  , documentEventTarget
  , bodyEventTarget
  
  , EventInfo
  , EventExecIn
  , _event
  , eventUpdate

  , UIEventRow
  , _uiEvent
  , reduceUIEventRow
  , uiEventUpdate

  , MouseEventRow
  , _mouseEvent
  , _posInTarget
  , reduceMouseEventRow
  , mouseEventUpdate
  , mousemove
  , click
  , mousedown
  , mouseup
  
  , KeyboardEventRow
  , _keyboardEvent
  , reduceKeyboardEventRow
  , keyboardEventUpdate
  , keydown
  , keyup
  , keypressed

  , change
  , load
  ) where


import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse_)
import Data.Vector.Polymorphic (Vector2, (><))
import Effect.Aff (effectCanceler, makeAff, runAff_)
import Game (GameUpdate(..), runReducer)
import Game.Aff (AffGameUpdate, _end, _env, _stateRef)
import Game.Aff.Web.Util (qSel)
import Game.Util (maybeThrow, newRef, readRef, runStateWithRef, writeRef)
import Game.Util.Maybe (liftBoth)
import Run (AFF, EFFECT, Run, expand, liftAff, liftEffect, runBaseAff')
import Run.Choose (CHOOSE, runChoose)
import Run.Except (EXCEPT, Except, FAIL, runExceptAt, throwAt)
import Run.Reader (READER, Reader, askAt, runReaderAt)
import Run.State (STATE, execState)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Web.DOM.Element (toEventTarget) as Element
import Web.DOM.ParentNode (QuerySelector)
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event (currentTarget) as Event
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLDocument (toEventTarget) as HTMLDocument
import Web.HTML.HTMLElement (toEventTarget, getBoundingClientRect, fromEventTarget) as HTMLElement
import Web.HTML.Window (document)
import Web.HTML.Window (toEventTarget) as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.UIEvent (UIEvent)
import Web.UIEvent.UIEvent as UIEvent


qSelEventTarget ∷
  ∀ r. QuerySelector → Run (EFFECT + FAIL + r) EventTarget
qSelEventTarget sel = qSel sel <#> Element.toEventTarget

windowEventTarget ∷ ∀ r. Run (EFFECT + r) EventTarget
windowEventTarget = liftEffect do window <#> Window.toEventTarget

documentEventTarget ∷ ∀ r. Run (EFFECT + r) EventTarget
documentEventTarget = liftEffect do
  window >>= document <#> HTMLDocument.toEventTarget

bodyEventTarget ∷ ∀ r. Run (EFFECT + FAIL + r) EventTarget
bodyEventTarget = liftBoth do
  window >>= document >>= body <#> map HTMLElement.toEventTarget


type EventInfo =
  { eventType ∷ EventType
  , useCapture ∷ Boolean
  }

type EventExecIn env state err a r =
  STATE state + EFFECT + AFF + EXCEPT err +
  ( env ∷ Reader env
  , event ∷ Reader Event
  , end ∷ Except a
  | r )

-- todo: rename to `GetEventTarget`?
type EventTargetRow env state err a =
  STATE state + EFFECT + AFF + EXCEPT err + CHOOSE +
  ( env ∷ Reader env
  , end ∷ Except a
  )

_event ∷ Proxy "event"
_event = Proxy

eventUpdate ∷
  ∀ env state err a extra
  . EventInfo
  → Run (EventTargetRow env state err a) EventTarget
  → Run (EventExecIn env state err a extra) Unit
  → AffGameUpdate extra env state err a
-- eventUpdate { eventType, useCapture } eventTargetsM update = do
--   targets ← getTargets
eventUpdate { eventType, useCapture } targets update =
  GameUpdate \reducer → do
    stateRef ← askAt _stateRef
    env ← askAt _env
    targetArray ← targets
      # (runChoose ∷ Run _ _ → Run _ (Array _))
      # runStateWithRef stateRef
      # expand
    result ← liftAff $ makeAff \cb → do
      listenerRef ← newRef Nothing
      let
        canceler = readRef listenerRef >>= traverse_ \l →
          for_ targetArray do removeEventListener eventType l useCapture
      listener ← eventListener \event → do
        state ← readRef stateRef
        (runReducer reducer update ∷ Run (EventExecIn env state err a ()) Unit)
          # runReaderAt _event event
          # runReaderAt _env env
          # execState state
          # runExceptAt _end
          # runBaseAff'
          # runAff_ case _ of
          -- todo: i think this can be written nicer. all the handling might not
          -- need to be deferred until the end
              Right (Right newState) → writeRef newState stateRef
              Right (Left  end     ) → canceler *> cb (Right end)
              Left         error     → throwError error
      writeRef (Just listener) listenerRef
      for_ targetArray do addEventListener eventType listener useCapture
      pure (effectCanceler canceler)
    throwAt _end result


type UIEventRow r = (uiEvent ∷ READER UIEvent | r)

_uiEvent ∷ SProxy "uiEvent"
_uiEvent = SProxy

reduceUIEventRow ∷
  ∀ a r
  . Run (UIEventRow (event ∷ READER Event, effect ∷ EFFECT | r)) a
  → Run             (event ∷ READER Event, effect ∷ EFFECT | r)  a
reduceUIEventRow uiEventRow = do
  event ← askAt _event
  uiEvent ← UIEvent.fromEvent event
    # maybeThrow "This Event is not a UIEvent"
  uiEventRow
    # runReaderAt _uiEvent uiEvent

-- | Boolean is whether to use capture
uiEventUpdate ∷
  ∀ extra e s a
  . EventType
  → Boolean
  → Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a (UIEventRow extra)) Unit
  → AffGameUpdate extra e s a
uiEventUpdate eventType useCapture targets update = eventUpdate
  { eventType, useCapture }
  targets
  (reduceUIEventRow update)


type MouseEventRow r =
  ( uiEvent     ∷ READER UIEvent
  , mouseEvent  ∷ READER MouseEvent
  , posInTarget ∷ READER (Vector2 Int)
  | r )

_mouseEvent ∷ SProxy "mouseEvent"
_mouseEvent = SProxy

_posInTarget ∷ SProxy "posInTarget"
_posInTarget = SProxy

-- | Assumes that the relevant `Event` is a `MouseEvent` (and `UIEvent`). The
-- | current event target (`event.currentTarget`) must be a `HTMLElement`.
-- | This function is not intended for use with events that do not meet these
-- | criteria.
reduceMouseEventRow ∷
  ∀ a r
  . Run (MouseEventRow (event ∷ READER Event, effect ∷ EFFECT | r)) a
  → Run                (event ∷ READER Event, effect ∷ EFFECT | r)  a
reduceMouseEventRow mouseEventRow = do
  event ← askAt _event
  uiEvent ← UIEvent.fromEvent event
    # maybeThrow "This Event is not a UIEvent"
  mouseEvent ← MouseEvent.fromEvent event
    # maybeThrow "This Event is not a MouseEvent"
  target ← Event.currentTarget event >>= HTMLElement.fromEventTarget
    # maybeThrow "This event.currentTarget is not a HTMLElement"
  { left, top } ← liftEffect $ HTMLElement.getBoundingClientRect target
  let
    clientPos = ((><) <$> MouseEvent.clientX <*> MouseEvent.clientY) mouseEvent
    posInTarget = (\p b → p - round b) <$> clientPos <*> (left >< top)
  mouseEventRow
    # runReaderAt _uiEvent uiEvent
    # runReaderAt _mouseEvent mouseEvent
    # runReaderAt _posInTarget posInTarget

mouseEventUpdate ∷
  ∀ extra e s a
  . EventType
  → Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a (MouseEventRow extra)) Unit
  → AffGameUpdate extra e s a
mouseEventUpdate eventType targets update = eventUpdate
  { eventType, useCapture: false }
  targets
  (reduceMouseEventRow update)

mousemove ∷
  ∀ extra e s a
  . Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a (MouseEventRow extra)) Unit
  → AffGameUpdate extra e s a
mousemove = mouseEventUpdate (EventType "mousemove")

click ∷
  ∀ extra e s a
  . Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a (MouseEventRow extra)) Unit
  → AffGameUpdate extra e s a
click = mouseEventUpdate (EventType "click")

mousedown ∷
  ∀ extra e s a
  . Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a (MouseEventRow extra)) Unit
  → AffGameUpdate extra e s a
mousedown = mouseEventUpdate (EventType "mousedown")

mouseup ∷
  ∀ extra e s a
  . Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a (MouseEventRow extra)) Unit
  → AffGameUpdate extra e s a
mouseup = mouseEventUpdate (EventType "mouseup")


type KeyboardEventRow r =
  ( uiEvent ∷ READER UIEvent
  , keyboardEvent ∷ READER KeyboardEvent
  | r )

_keyboardEvent ∷ SProxy "keyboardEvent"
_keyboardEvent = SProxy

-- | Assumes that the relevant `Event` is a `KeyboardEvent` (and `UIEvent`).
-- | Throws an error when attempting to read a value that doesn't exist on the
-- | `Event`. This function is not intended for use with events that are not
-- | `KeyboardEvent`s.
reduceKeyboardEventRow ∷
  ∀ a r
  . Run (KeyboardEventRow (event ∷ READER Event, effect ∷ EFFECT | r)) a
  → Run                   (event ∷ READER Event, effect ∷ EFFECT | r)  a
reduceKeyboardEventRow keyboardEventRow = do
  event ← askAt _event
  uiEvent ← UIEvent.fromEvent event
    # maybeThrow "This Event is not UIEvent"
  keyboardEvent ← KeyboardEvent.fromEvent event
    # maybeThrow "This Event is not KeyboardEvent"
  keyboardEventRow
    # runReaderAt _uiEvent uiEvent
    # runReaderAt _keyboardEvent keyboardEvent

keyboardEventUpdate ∷
  ∀ extra e s a
  . EventType
  → Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a (KeyboardEventRow extra)) Unit
  → AffGameUpdate extra e s a
keyboardEventUpdate eventType targets update = eventUpdate
  { eventType, useCapture: false }
  targets
  (reduceKeyboardEventRow update)

keydown ∷
  ∀ extra e s a
  . Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a (KeyboardEventRow extra)) Unit
  → AffGameUpdate extra e s a
keydown = keyboardEventUpdate (EventType "keydown")

keyup ∷
  ∀ extra e s a
  . Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a (KeyboardEventRow extra)) Unit
  → AffGameUpdate extra e s a
keyup = keyboardEventUpdate (EventType "keyup")

keypressed ∷
  ∀ extra e s a
  . Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a (KeyboardEventRow extra)) Unit
  → AffGameUpdate extra e s a
keypressed = keyboardEventUpdate (EventType "keypressed")


change ∷
  ∀ extra e s a
  . Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a extra) Unit
  → AffGameUpdate extra e s a
change = eventUpdate { eventType: EventType "change", useCapture: false }

load ∷
  ∀ extra e s a
  . Run (EventTargetRow e s a) EventTarget
  → Run (EventExecIn e s a extra) Unit
  → AffGameUpdate extra e s a
load = eventUpdate { eventType: EventType "load", useCapture: false }
