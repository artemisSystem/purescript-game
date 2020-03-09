module Game.Aff.Web.EventHelpers where


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
import Game.Web
import Game.Web.Util
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventTarget)
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
  :: forall s a
   . QuerySelector
  -> (Event -> Ref s -> Effect (Maybe a))
  -> Effect (GameEvent s a)
clickEvent elem update = ado
  target <- qSel elem <#> map toEventTarget >>= case _ of
    Just target -> pure target
    Nothing -> throwError (error "Element for clickEvent not found")
  in GameEvent
    { eventType: EventType "click"
    , target
    , update
    , useCapture: false
    }


target :: QuerySelector -> Effect (Maybe EventTarget)
target = qSel >>> (map <<< map) toEventTarget

allTargets :: QuerySelector -> Effect (Array EventTarget)
allTargets = qSelAll >>> (map <<< map) toEventTarget

{- 
mkInputUpdate
  :: forall m t v s a
   . Traversable t
  => MonadEffect m
  => (HTMLInputElement -> Effect v)
  -> t QuerySelector
  -> (t v -> Ref s -> m (Maybe a))
  -> (Ref s -> m (Maybe a))
 -}
 
{- 
inputChangeEvent
  :: forall t v s a
   . Traversable t
  => (HTMLInputElement -> Effect v)
  -> t QuerySelector
  -> (t v -> EffectUpdate s a)
  -> Effect (t (GameEvent s a))
inputChangeEvent getValue inputSelectors update = ado
  inputs <- qSel <$> inputSelectors
    # (map sequence <<< sequence)
    # (map <<< (=<<) <<< traverse) Input.fromElement
    # (=<<) case _ of
        Just is -> pure is
        Nothing -> throwError (error "Input for inputEvent not found")
  in inputs <#> Input.toEventTarget <#> \target ->
    { eventType: EventType "change"
    , target
    , update: \_ -> do
        values <- traverse getValue inputs
        update values
    , useCapture: false
    }

inputChangeEventValue
  :: forall t s a
   . Traversable t
  => t QuerySelector
  -> (t String -> EffectUpdate s a)
  -> Effect (List (GameEvent s a))
inputChangeEventValue = inputChangeEvent Input.value

inputChangeEventNumber
  :: forall t s a
   . Traversable t
  => t QuerySelector
  -> (t Number -> EffectUpdate s a)
  -> Effect (List (GameEvent s a))
inputChangeEventNumber = inputChangeEvent Input.valueAsNumber

inputChangeEventOne
  :: forall v s a
   . (HTMLInputElement -> Effect v)
  -> QuerySelector
  -> (v -> EffectUpdate s a)
  -> Effect (GameEvent s a)
inputChangeEventOne getValue inputSelector update = unsafePartial head <$>
  inputChangeEvent getValue (Identity inputSelector) (unwrap >>> update)

inputChangeEventValueOne
  :: forall s a
   . QuerySelector
  -> (String -> EffectUpdate s a)
  -> Effect (GameEvent s a)
inputChangeEventValueOne = inputChangeEventOne Input.value

inputChangeEventNumberOne
  :: forall s a
   . QuerySelector
  -> (Number -> EffectUpdate s a)
  -> Effect (GameEvent s a)
inputChangeEventNumberOne = inputChangeEventOne Input.valueAsNumber

-- TODO: docs about how this doesnt run the update function of the original event
-- TODO: instead, maybe have a function that makes an event that fires on event A, but runs event B's update
