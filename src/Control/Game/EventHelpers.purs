module Control.Game.EventHelpers where

import Prelude

import Control.Game (GameEvent)
import Control.Monad.Error.Class (throwError)
import Data.List (List, fromFoldable, (:))
import Data.List.Partial (head)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable, traverse, sequence)
import Effect (Effect)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)
import Web.DOM.Element (toEventTarget, Element)
import Web.DOM.ParentNode (QuerySelector, querySelector)
import Web.Event.Event (Event, EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement (fromElement, toEventTarget, value, valueAsNumber) as Input
import Web.HTML.Window (document)

-- TODO:
-- Note: `Kleisli m a` is being used to denote a function of type (a -> m a),
-- not assuming the actual newtype (called Star) will be used
-- mouse clicks: (event -> Kleisli effect)
-- mouse clicks: (position -> Kleisli effect)
-- mouse clicks: (position within some element -> Kleisli effect)
-- mouse movements: (event -> Kleisli effect)
-- mouse movements: (position -> Kleisli effect)
-- mouse movements: (position within some element -> Kleisli effect)
-- keyboard inputs: Foldable (Key -> Kleisli Effect state)
-- keyboard inputs: (event -> Kleisli effect)

-- | `querySelector` without having to supply a `ParentNode`, using the
-- | document as parent node.
qSel :: QuerySelector -> Effect (Maybe Element)
qSel sel = do
  doc <- window >>= document <#> toParentNode
  querySelector sel doc

-- | Create a `GameEvent` that fires when the specified button is clicked
buttonEvent
  :: forall state
   . QuerySelector
  -> (Event -> state -> Effect state)
  -> Effect (GameEvent state)
buttonEvent button update = do
  target <- qSel button <#> map toEventTarget >>= case _ of
    Just elem -> pure elem
    Nothing -> throwError (error "Button for buttonEvent not found")
  pure
    { eventType: EventType "click"
    , target
    , update
    , useCapture: false
    }

inputChangeEvent
  :: forall t a state
   . Traversable t
  => (HTMLInputElement -> Effect a)
  -> t QuerySelector
  -> (t a -> state -> Effect state)
  -> Effect (List (GameEvent state))
inputChangeEvent getValue inputSelectors update = ado
  inputs <- qSel <$> inputSelectors
    # (map sequence <<< sequence)
    # (map <<< (=<<) <<< traverse) Input.fromElement
    # (=<<) case _ of
      Just is -> pure is
      Nothing -> throwError (error "Input for inputEvent not found")
  in fromFoldable inputs <#> Input.toEventTarget <#> \target ->
    { eventType: EventType "change"
    , target
    , update: \_ state -> do
        values <- traverse getValue inputs
        update values state
    , useCapture: false
    }

inputChangeEventValue
  :: forall t state
   . Traversable t
  => t QuerySelector
  -> (t String -> state -> Effect state)
  -> Effect (List (GameEvent state))
inputChangeEventValue = inputChangeEvent Input.value

inputChangeEventNumber
  :: forall t state
   . Traversable t
  => t QuerySelector
  -> (t Number -> state -> Effect state)
  -> Effect (List (GameEvent state))
inputChangeEventNumber = inputChangeEvent Input.valueAsNumber

inputChangeEventOne
  :: forall a state
   . (HTMLInputElement -> Effect a)
  -> QuerySelector
  -> (a -> state -> Effect state)
  -> Effect (GameEvent state)
inputChangeEventOne getValue inputSelector update = unsafePartial head <$>
  inputChangeEvent getValue (Identity inputSelector) (unwrap >>> update)

inputChangeEventValueOne
  :: forall state
   . QuerySelector
  -> (String -> state -> Effect state)
  -> Effect (GameEvent state)
inputChangeEventValueOne = inputChangeEventOne Input.value

inputChangeEventNumberOne
  :: forall state
   . QuerySelector
  -> (Number -> state -> Effect state)
  -> Effect (GameEvent state)
inputChangeEventNumberOne = inputChangeEventOne Input.valueAsNumber

inputEvent
  :: forall t a state
   . Traversable t
  => (HTMLInputElement -> Effect a)
  -> GameEvent state
  -> t QuerySelector
  -> (t a -> state -> Effect state)
  -> Effect (GameEvent state)
inputEvent getValue baseEvent inputSelectors update = ado
  { update } <- inputChangeEvent getValue inputSelectors update >>= case _ of
    (x:_) -> pure x
    _ -> throwError (error "Traversable for inputEvent was empty")
  in { eventType: baseEvent.eventType
     , target: baseEvent.target
     , update: \evt state -> do
        state' <- baseEvent.update evt state
        update undefined state'
     , useCapture: baseEvent.useCapture
     }

inputEventValue
  :: forall t state
   . Traversable t
  => GameEvent state
  -> t QuerySelector
  -> (t String -> state -> Effect state)
  -> Effect (GameEvent state)
inputEventValue = inputEvent Input.value

inputEventNumber
  :: forall t state
   . Traversable t
  => GameEvent state
  -> t QuerySelector
  -> (t Number -> state -> Effect state)
  -> Effect (GameEvent state)
inputEventNumber = inputEvent Input.valueAsNumber

inputEventOne
  :: forall a state
   . (HTMLInputElement -> Effect a)
  -> GameEvent state
  -> QuerySelector
  -> (a -> state -> Effect state)
  -> Effect (GameEvent state)
inputEventOne getValue baseEvent inputSelector update =
  inputEvent getValue baseEvent (Identity inputSelector) (unwrap >>> update)

inputEventValueOne
  :: forall state
   . GameEvent state
  -> QuerySelector
  -> (String -> state -> Effect state)
  -> Effect (GameEvent state)
inputEventValueOne = inputEventOne Input.value

inputEventNumberOne
  :: forall state
   . GameEvent state
  -> QuerySelector
  -> (Number -> state -> Effect state)
  -> Effect (GameEvent state)
inputEventNumberOne = inputEventOne Input.valueAsNumber
