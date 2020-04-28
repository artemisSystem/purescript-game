module Game.Aff.Web.Event
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

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Int (round)
import Data.Traversable (class Foldable, for_, traverse_)
import Data.Vector.Polymorphic (Vector2, (><))
import Effect.Aff (effectCanceler, makeAff)
import Game (mkUpdate')
import Game.Aff (AffGameUpdate, ExecOut, _end, _stateRef, _env)
import Game.Aff.Web.Util (qSel)
import Game.Util (maybeThrow, newRef, readRef, writeRef)
import Game.Util.Maybe (liftBoth)
import Prim.Row (class Nub, class Union)
import Run (EFFECT, Run, SProxy(..), liftAff, liftEffect, runBaseEffect)
import Run.Except (EXCEPT, FAIL, runExceptAt, throwAt)
import Run.Reader (READER, askAt, runReaderAt)
import Run.State (STATE, execState)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (toEventTarget) as Element
import Web.DOM.ParentNode (QuerySelector)
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event (currentTarget) as Event
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.Window (toEventTarget) as Window
import Web.HTML.HTMLElement (toEventTarget, getBoundingClientRect, fromEventTarget) as HTMLElement
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLDocument (toEventTarget) as HTMLDocument
import Web.UIEvent.UIEvent (UIEvent)
import Web.UIEvent.UIEvent as UIEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent


qSelEventTarget
  :: forall r
   . QuerySelector
  -> Run (effect :: EFFECT, except :: FAIL | r) EventTarget
qSelEventTarget sel = qSel sel <#> Element.toEventTarget

windowEventTarget :: forall r. Run (effect :: EFFECT | r) EventTarget
windowEventTarget = liftEffect do window <#> Window.toEventTarget

documentEventTarget :: forall r. Run (effect :: EFFECT | r) EventTarget
documentEventTarget = liftEffect do
  window >>= document <#> HTMLDocument.toEventTarget

bodyEventTarget
  :: forall r. Run (effect :: EFFECT, except :: FAIL | r) EventTarget
bodyEventTarget = liftBoth do
  window >>= document >>= body <#> map HTMLElement.toEventTarget


type EventInfo =
  { eventType :: EventType
  , useCapture :: Boolean
  }

type EventExecIn e s a r =
  ( state  :: STATE s
  , env    :: READER e
  , end    :: EXCEPT a
  , event  :: READER Event
  , effect :: EFFECT
  | r )

_event :: SProxy "event"
_event = SProxy

eventUpdate
  :: forall f r e s a extra update
   . Foldable f
  => Union (EventExecIn e s a r) extra update
  => Nub (EventExecIn e s a                     r ) (EventExecIn e s a r)
  => Nub (EventExecIn e s a (effect :: EFFECT | r)) (EventExecIn e s a r)
  => EventInfo
  -> (Run (EventExecIn e s a r) Unit -> Run (EventExecIn e s a ()) Unit)
  -> f EventTarget
  -> Run update Unit
  -> AffGameUpdate extra e s a
eventUpdate { eventType, useCapture } reduce targets = mkUpdate' $
  coerce \execIn -> do
    stateRef <- askAt _stateRef
    env <- askAt _env
    result <- liftAff $ makeAff \cb -> do
      listenerRef <- newRef Nothing
      let canceler = readRef listenerRef >>= traverse_ \l ->
            for_ targets do removeEventListener eventType l useCapture
      listener <- eventListener \event -> do
        state <- readRef stateRef
        r <- reduce execIn
          # runReaderAt _event event
          # runReaderAt _env env
          # execState state
          # runExceptAt _end
          # runBaseEffect
        case r of
          Right newState -> writeRef newState stateRef
          Left end -> canceler *> cb (Right end)
      writeRef (Just listener) listenerRef
      for_ targets do addEventListener eventType listener useCapture
      pure (effectCanceler canceler)
    throwAt _end result
  where
    coerce :: (Run (EventExecIn e s a r) Unit -> Run (ExecOut e s a) Unit)
           -> (Run _                     Unit -> Run (ExecOut e s a) Unit)
    coerce = unsafeCoerce


type UIEventRow r = (uiEvent :: READER UIEvent | r)

_uiEvent :: SProxy "uiEvent"
_uiEvent = SProxy

reduceUIEventRow
  :: forall e s a r b
   . Run (EventExecIn e s a (UIEventRow r)) b
  -> Run (EventExecIn e s a             r ) b
reduceUIEventRow uiEventRow = do
  event <- askAt _event
  uiEvent <- UIEvent.fromEvent event
    # maybeThrow "This Event is not a UIEvent"
  uiEventRow
    # runReaderAt _uiEvent uiEvent

-- | Boolean is whether to use capture
uiEventUpdate
  :: forall f extra e s a
   . Foldable f
  => EventType
  -> Boolean
  -> f EventTarget
  -> Run (EventExecIn e s a (UIEventRow extra)) Unit
  -> AffGameUpdate extra e s a
uiEventUpdate eventType useCapture = eventUpdate
  { eventType, useCapture }
  reduceUIEventRow


type MouseEventRow r =
  ( uiEvent :: READER UIEvent
  , mouseEvent :: READER MouseEvent
  , posInTarget :: READER (Vector2 Int)
  | r )

_mouseEvent :: SProxy "mouseEvent"
_mouseEvent = SProxy

_posInTarget :: SProxy "posInTarget"
_posInTarget = SProxy

-- | Assumes that the relevant `Event` is a `MouseEvent` (and `UIEvent`). The
-- | current event target (`event.currentTarget`) must be a `HTMLElement`.
-- | This function is not intended for use with events that do not meet these
-- | criteria.
reduceMouseEventRow
  :: forall e s a r b
   . Run (EventExecIn e s a (MouseEventRow r)) b
  -> Run (EventExecIn e s a                r ) b
reduceMouseEventRow mouseEventRow = do
  event <- askAt _event
  uiEvent <- UIEvent.fromEvent event
    # maybeThrow "This Event is not a UIEvent"
  mouseEvent <- MouseEvent.fromEvent event
    # maybeThrow "This Event is not a MouseEvent"
  target <- Event.currentTarget event >>= HTMLElement.fromEventTarget
    # maybeThrow "This event.currentTarget is not a HTMLElement"
  { left, top } <- liftEffect $ HTMLElement.getBoundingClientRect target
  let
    clientPos = ((><) <$> MouseEvent.clientX <*> MouseEvent.clientY) mouseEvent
    posInTarget = (\p b -> p - round b) <$> clientPos <*> (left >< top)
  mouseEventRow
    # runReaderAt _uiEvent uiEvent
    # runReaderAt _mouseEvent mouseEvent
    # runReaderAt _posInTarget posInTarget

mouseEventUpdate
  :: forall f extra e s a
   . Foldable f
  => EventType
  -> f EventTarget
  -> Run (EventExecIn e s a (MouseEventRow extra)) Unit
  -> AffGameUpdate extra e s a
mouseEventUpdate eventType = eventUpdate
  { eventType, useCapture: false }
  reduceMouseEventRow

mousemove
  :: forall f extra e s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn e s a (MouseEventRow extra)) Unit
  -> AffGameUpdate extra e s a
mousemove = mouseEventUpdate (EventType "mousemove")

click
  :: forall f extra e s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn e s a (MouseEventRow extra)) Unit
  -> AffGameUpdate extra e s a
click = mouseEventUpdate (EventType "click")

mousedown
  :: forall f extra e s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn e s a (MouseEventRow extra)) Unit
  -> AffGameUpdate extra e s a
mousedown = mouseEventUpdate (EventType "mousedown")

mouseup
  :: forall f extra e s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn e s a (MouseEventRow extra)) Unit
  -> AffGameUpdate extra e s a
mouseup = mouseEventUpdate (EventType "mouseup")


type KeyboardEventRow r =
  ( uiEvent :: READER UIEvent
  , keyboardEvent :: READER KeyboardEvent
  | r )

_keyboardEvent :: SProxy "keyboardEvent"
_keyboardEvent = SProxy

-- | Assumes that the relevant `Event` is a `KeyboardEvent` (and `UIEvent`).
-- | Throws an error when attempting to read a value that doesn't exist on the
-- | `Event`. This function is not intended for use with events that are not
-- | `KeyboardEvent`s.
reduceKeyboardEventRow
  :: forall e s a r b
   . Run (EventExecIn e s a (KeyboardEventRow r)) b
  -> Run (EventExecIn e s a                   r ) b
reduceKeyboardEventRow keyboardEventRow = do
  event <- askAt _event
  uiEvent <- UIEvent.fromEvent event
    # maybeThrow "This Event is not UIEvent"
  keyboardEvent <- KeyboardEvent.fromEvent event
    # maybeThrow "This Event is not KeyboardEvent"
  keyboardEventRow
    # runReaderAt _uiEvent uiEvent
    # runReaderAt _keyboardEvent keyboardEvent

keyboardEventUpdate
  :: forall f extra e s a
   . Foldable f
  => EventType
  -> f EventTarget
  -> Run (EventExecIn e s a (KeyboardEventRow extra)) Unit
  -> AffGameUpdate extra e s a
keyboardEventUpdate eventType = eventUpdate
  { eventType, useCapture: false }
  reduceKeyboardEventRow

keydown
  :: forall f extra e s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn e s a (KeyboardEventRow extra)) Unit
  -> AffGameUpdate extra e s a
keydown = keyboardEventUpdate (EventType "keydown")

keyup
  :: forall f extra e s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn e s a (KeyboardEventRow extra)) Unit
  -> AffGameUpdate extra e s a
keyup = keyboardEventUpdate (EventType "keyup")

keypressed
  :: forall f extra e s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn e s a (KeyboardEventRow extra)) Unit
  -> AffGameUpdate extra e s a
keypressed = keyboardEventUpdate (EventType "keypressed")


change
  :: forall f extra e s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn e s a extra) Unit
  -> AffGameUpdate extra e s a
change = eventUpdate
  { eventType: EventType "change", useCapture: false }
  identity

load
  :: forall f extra e s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn e s a extra) Unit
  -> AffGameUpdate extra e s a
load = eventUpdate
  { eventType: EventType "load", useCapture: false }
  identity
