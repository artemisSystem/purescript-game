module Control.Game.Web.EventHelpers where

{-
import Prelude

import Control.Game (GameEvent)
import Control.Game.Util (qSel)
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
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector)
import Web.Event.Event (Event, EventType(..))
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement (fromElement, toEventTarget, value, valueAsNumber) as Input

-- TODO:
-- Note: `Kleisli m a` is being used to denote a function of type (a -> m a),
-- not inferring the actual newtype (called Star) will be used
-- mouse clicks: (event -> Kleisli effect)
-- mouse clicks: (position -> Kleisli effect)
-- mouse clicks: (position within some element -> Kleisli effect)
-- mouse movements: (event -> Kleisli effect)
-- mouse movements: (position -> Kleisli effect)
-- mouse movements: (position within some element -> Kleisli effect)
-- keyboard inputs: Foldable (Key -> Kleisli Effect state)
-- keyboard inputs: (event -> Kleisli effect)
-- document onload

-- | Create a `GameEvent` that fires when the specified element is clicked
clickEvent
  :: forall state
   . QuerySelector
  -> (Event -> state -> Effect state)
  -> Effect (GameEvent state)
clickEvent elem update = ado
  target <- qSel elem <#> map toEventTarget >>= case _ of
    Just target -> pure target
    Nothing -> throwError (error "Element for clickEvent not found")
  in { eventType: EventType "click"
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

-- TODO: docs about how this doesnt run the update function of the original event
inputEvent
  :: forall t a state
   . Traversable t
  => (HTMLInputElement -> Effect a)
  -> GameEvent state
  -> t QuerySelector
  -> (t a -> state -> Effect state)
  -> Effect (GameEvent state)
inputEvent getValue baseEvent inputSelectors update = ado
  changeEvent <- inputChangeEvent getValue inputSelectors update >>= case _ of
    (x:_) -> pure x
    _ -> throwError (error "Traversable for inputEvent was empty")
  in { eventType: baseEvent.eventType
     , target: baseEvent.target
     , update: \_ state -> changeEvent.update (undefined :: Event) state
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
