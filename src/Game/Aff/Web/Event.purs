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

  , forInputs
  , forInputs_
  ) where


import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Int (round)
import Data.Tuple (Tuple(..))
import Data.Traversable (class Foldable, class Traversable, for, for_, traverse_)
import Data.Vector.Polymorphic (Vector2, (><))
import Effect (Effect)
import Effect.Aff (effectCanceler, makeAff)
import Game (mkUpdate)
import Game.Aff (AffGameUpdate, ExecOut, _end, _stateRef)
import Game.Aff.Web.Util (qSel)
import Game.Util (maybeThrow, newRef, readRef, writeRef)
import Prim.Row (class Nub, class Union)
import Record.Extra (class MapRecord, class SequenceRecord, class ZipRecord, mapRecord, sequenceRecord, zipRecord)
import Run (EFFECT, Run, SProxy(..), liftAff, liftEffect, runBaseEffect)
import Run.Except (EXCEPT, runExceptAt, throwAt)
import Run.Reader (READER, askAt, runReaderAt)
import Run.State (STATE, execState, runStateAt)
import Type.Data.Row (RProxy)
import Type.Equality (class TypeEquals)
import Type.RowList (class RowToList)
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
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement (setValue, setValueAsNumber, value, valueAsNumber) as Input
import Web.UIEvent.UIEvent (UIEvent)
import Web.UIEvent.UIEvent as UIEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent


qSelEventTarget :: QuerySelector -> Effect (Maybe EventTarget)
qSelEventTarget sel = qSel sel <#> map Element.toEventTarget

windowEventTarget :: Effect EventTarget
windowEventTarget = window <#> Window.toEventTarget

documentEventTarget :: Effect EventTarget
documentEventTarget = window >>= document <#> HTMLDocument.toEventTarget

bodyEventTarget :: Effect (Maybe EventTarget)
bodyEventTarget = window >>= document >>= body <#> map HTMLElement.toEventTarget


type EventInfo =
  { eventType :: EventType
  , useCapture :: Boolean
  }

type EventExecIn s a r =
  ( state  :: STATE s
  , end    :: EXCEPT a
  , event  :: READER Event
  , effect :: EFFECT
  | r )

_event :: SProxy "event"
_event = SProxy

eventUpdate
  :: forall f r s a extra update
   . Foldable f
  => Union (EventExecIn s a r) extra update
  => Nub (EventExecIn s a                     r ) (EventExecIn s a r)
  => Nub (EventExecIn s a (effect :: EFFECT | r)) (EventExecIn s a r)
  => EventInfo
  -> (Run (EventExecIn s a r) Unit -> Run (EventExecIn s a ()) Unit)
  -> f EventTarget
  -> Run update Unit
  -> AffGameUpdate extra s a
eventUpdate { eventType, useCapture } reduce targets = mkUpdate $
  coerce \execIn -> do
    stateRef <- askAt _stateRef
    result <- liftAff $ makeAff \cb -> do
      listenerRef <- newRef Nothing
      let canceler = readRef listenerRef >>= traverse_ \l ->
            for_ targets do removeEventListener eventType l useCapture
      listener <- eventListener \event -> do
        state <- readRef stateRef
        r <- reduce execIn
          # runReaderAt _event event
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
    coerce :: (Run (EventExecIn s a r) Unit -> Run (ExecOut s a) Unit)
           -> (Run _                   Unit -> Run (ExecOut s a) Unit)
    coerce = unsafeCoerce


type UIEventRow r = (uiEvent :: READER UIEvent | r)

_uiEvent :: SProxy "uiEvent"
_uiEvent = SProxy

reduceUIEventRow
  :: forall s a r b
   . Run (EventExecIn s a (UIEventRow r)) b
  -> Run (EventExecIn s a             r ) b
reduceUIEventRow uiEventRow = do
  event <- askAt _event
  uiEvent <- UIEvent.fromEvent event
    # maybeThrow "This Event is not a UIEvent"
  uiEventRow
    # runReaderAt _uiEvent uiEvent

-- | Boolean is whether to use capture
uiEventUpdate
  :: forall f extra s a
   . Foldable f
  => EventType
  -> Boolean
  -> f EventTarget
  -> Run (EventExecIn s a (UIEventRow extra)) Unit
  -> AffGameUpdate extra s a
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
  :: forall s a r b
   . Run (EventExecIn s a (MouseEventRow r)) b
  -> Run (EventExecIn s a                r ) b
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
  :: forall f extra s a
   . Foldable f
  => EventType
  -> f EventTarget
  -> Run (EventExecIn s a (MouseEventRow extra)) Unit
  -> AffGameUpdate extra s a
mouseEventUpdate eventType = eventUpdate
  { eventType, useCapture: false }
  reduceMouseEventRow

mousemove
  :: forall f extra s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn s a (MouseEventRow extra)) Unit
  -> AffGameUpdate extra s a
mousemove = mouseEventUpdate (EventType "mousemove")

click
  :: forall f extra s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn s a (MouseEventRow extra)) Unit
  -> AffGameUpdate extra s a
click = mouseEventUpdate (EventType "click")

mousedown
  :: forall f extra s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn s a (MouseEventRow extra)) Unit
  -> AffGameUpdate extra s a
mousedown = mouseEventUpdate (EventType "mousedown")

mouseup
  :: forall f extra s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn s a (MouseEventRow extra)) Unit
  -> AffGameUpdate extra s a
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
  :: forall s a r b
   . Run (EventExecIn s a (KeyboardEventRow r)) b
  -> Run (EventExecIn s a                   r ) b
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
  :: forall f extra s a
   . Foldable f
  => EventType
  -> f EventTarget
  -> Run (EventExecIn s a (KeyboardEventRow extra)) Unit
  -> AffGameUpdate extra s a
keyboardEventUpdate eventType = eventUpdate
  { eventType, useCapture: false }
  reduceKeyboardEventRow

keydown
  :: forall f extra s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn s a (KeyboardEventRow extra)) Unit
  -> AffGameUpdate extra s a
keydown = keyboardEventUpdate (EventType "keydown")

keyup
  :: forall f extra s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn s a (KeyboardEventRow extra)) Unit
  -> AffGameUpdate extra s a
keyup = keyboardEventUpdate (EventType "keyup")

keypressed
  :: forall f extra s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn s a (KeyboardEventRow extra)) Unit
  -> AffGameUpdate extra s a
keypressed = keyboardEventUpdate (EventType "keypressed")


change
  :: forall f extra s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn s a extra) Unit
  -> AffGameUpdate extra s a
change = eventUpdate
  { eventType: EventType "change", useCapture: false }
  identity

load
  :: forall f extra s a
   . Foldable f
  => f EventTarget
  -> Run (EventExecIn s a extra) Unit
  -> AffGameUpdate extra s a
load = eventUpdate
  { eventType: EventType "load", useCapture: false }
  identity


type UpdateInputsRow values valueAsNumbers r =
  ( value :: STATE (Record values)
  , valueAsNumber :: STATE (Record valueAsNumbers)
  , effect :: EFFECT
  | r )

_value :: SProxy "value"
_value = SProxy

_valueAsNumber :: SProxy "valueAsNumber"
_valueAsNumber = SProxy

updateInputs
  :: forall a extra
     inputs inputs2 values valueAsNumbers valuesE valueAsNumbersE
     in_rl in_rl2 v_rl vN_rl vE_rl vNE_rl
     valuesInputs valueAsNumbersInputs units unitsE
     vI_rl vNI_rl units_rl unitsE_rl
   . TypeEquals (RProxy inputs) (RProxy inputs2)
  => RowToList inputs in_rl
  => RowToList inputs2 in_rl2
  => RowToList values v_rl
  => RowToList valueAsNumbers vN_rl
  => RowToList valuesE vE_rl
  => RowToList valueAsNumbersE vNE_rl
  => RowToList valuesInputs vI_rl
  => RowToList valueAsNumbersInputs vNI_rl
  => RowToList unitsE unitsE_rl
  => RowToList units units_rl
  => MapRecord in_rl inputs HTMLInputElement (Effect String) () valuesE
  => SequenceRecord vE_rl valuesE () values Effect
  => MapRecord in_rl2 inputs2 HTMLInputElement (Effect Number) () valueAsNumbersE
  => SequenceRecord vNE_rl valueAsNumbersE () valueAsNumbers Effect
  => ZipRecord v_rl values in_rl inputs () valuesInputs
  => MapRecord vI_rl valuesInputs (Tuple String HTMLInputElement) (Effect Unit) () unitsE
  => SequenceRecord unitsE_rl unitsE () units Effect
  => ZipRecord vN_rl valueAsNumbers in_rl2 inputs2 () valueAsNumbersInputs
  => MapRecord vNI_rl valueAsNumbersInputs (Tuple Number HTMLInputElement) (Effect Unit) () unitsE
  => SequenceRecord unitsE_rl unitsE () units Effect
  => Record inputs
  -> Run (UpdateInputsRow values valueAsNumbers extra) a
  -> Run (effect :: EFFECT | extra) a
updateInputs inputs updateInputsRow = do
  values' <- values
  valueAsNumbers' <- valueAsNumbers
  updateInputsRow
    # runStateAt _value values'
    # runStateAt _valueAsNumber valueAsNumbers'
    # (=<<) do \(Tuple s a) -> setValueAsNumbers s $> a
    # (=<<) do \(Tuple s a) -> setValues s $> a
  where
    inputs2 :: Record inputs2
    inputs2 = unsafeCoerce inputs
    values = mapRecord Input.value inputs
      # sequenceRecord
      # liftEffect
    valueAsNumbers = mapRecord Input.valueAsNumber inputs2
      # sequenceRecord
      # liftEffect
    setValues vs = zipRecord vs inputs
      # mapRecord (\(Tuple v i) -> Input.setValue v i)
      # sequenceRecord
      # liftEffect
    setValueAsNumbers vs = zipRecord vs inputs2
      # mapRecord (\(Tuple v i) -> Input.setValueAsNumber v i)
      # sequenceRecord
      # liftEffect


type ForInputsRow r =
  ( value :: STATE String
  , valueAsNumber :: STATE Number
  , effect :: EFFECT
  | r )

forInputsImpl
  :: forall extra inputs a b
   . (  inputs
     -> (HTMLInputElement -> Run (effect :: EFFECT | extra) a)
     -> b )
  -> inputs
  -> Run (ForInputsRow extra) a
  -> b
forInputsImpl f inputs forInputsRow = f inputs \elem -> do
  value         <- liftEffect $ Input.value         elem
  valueAsNumber <- liftEffect $ Input.valueAsNumber elem
  forInputsRow
    # runStateAt _value value
    # runStateAt _valueAsNumber valueAsNumber
    # (=<<) do \(Tuple s a) -> liftEffect $ Input.setValueAsNumber s elem $> a
    # (=<<) do \(Tuple s a) -> liftEffect $ Input.setValue s elem $> a

forInputs
  :: forall t a extra
   . Traversable t
  => t HTMLInputElement
  -> Run (ForInputsRow extra) a
  -> Run (effect :: EFFECT | extra) (t a)
forInputs = forInputsImpl for

forInputs_
  :: forall f a extra
   . Foldable f
  => f HTMLInputElement
  -> Run (ForInputsRow extra) a
  -> Run (effect :: EFFECT | extra) Unit
forInputs_ = forInputsImpl for_
