module Control.Game
  ( GameEvent
  , Game, game
  , CanvasGame, canvasGame
  , noUpdate
  , noEnd
  , noEvents
--, noSignal
  ) where

import Prelude

import Control.Game.Util (qSel)
import Control.Monad.Error.Class (throwError)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Exception (Error, error)
import Effect.Now (now)
import Effect.Ref (new, read, write) as R
import Graphics.Canvas as C
import Graphics.CanvasAction (CanvasAction, CanvasActionM, runAction)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ParentNode (QuerySelector)
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (EventTarget)
import Web.HTML (window)
import Web.HTML.HTMLCanvasElement (fromElement, HTMLCanvasElement)
import Web.HTML.Window (cancelAnimationFrame, requestAnimationFrame)


-- | The type of events that can be used in `Game`
type GameEvent state =
  { eventType  :: EventType
  , target     :: EventTarget
  , update     :: Event -> state -> Effect state
  , useCapture :: Boolean
  }

-- | A record type containing the information needed to run a game loop with
-- | `game`
type Game state return =
  { init    :: Effect state
  , update  :: Seconds -> state -> Effect state
  , display :: state -> Effect Unit
  , end     :: state -> Effect (Maybe (Either Error return))
  , events  :: List (GameEvent state)
-- TODO:
--, signal  :: Signal (state -> Effect state)
  }

-- TODO: Rewrite to make use of Aff better?
-- | Make an `Aff` that will start your game loop when run
game :: forall s a. Game s a -> Aff a
game { init, update, display, end, events } = makeAff \cb -> do
  gameWindow <- window
  -- Mutable values
  state <- init >>= R.new
  rafID <- R.new Nothing
  -- Setup events
  let getListener { update: evtUpdate } = eventListener
        \event -> R.read state
              >>= evtUpdate event
              >>= flip R.write state
  manageEvents <- for events \event@{ eventType, target, useCapture } ->
    getListener event >>= \listener -> pure
      { add:       addEventListener eventType listener useCapture target
      , remove: removeEventListener eventType listener useCapture target
      }
  for_ manageEvents _.add
  let
  -- Define canceler as effect
    cancel = do
      id <- R.read rafID
      for_ id (_ `cancelAnimationFrame` gameWindow)
      for_ manageEvents _.remove
  -- Setup for requestAnimationFrame
    tick t0 = do
      -- Setup
      Milliseconds t <- unInstant <$> now
      let dt = Seconds ((t - t0) / 1000.0)
      -- Update and display state
      newState <- R.read state >>= update dt
      R.write newState state
      display newState
      -- Loop
      end newState >>= case _ of
        Just ea -> cancel *> cb ea
        Nothing -> do
          id <- requestAnimationFrame (tick t) gameWindow
          R.write (Just id) rafID
  Milliseconds t0 <- unInstant <$> now
  -- Initial requestAnimationFrame
  id0 <- requestAnimationFrame (tick t0) gameWindow
  R.write (Just id0) rafID
  -- Canceler
  pure (effectCanceler cancel)

-- | A record type containing the information needed to run a game loop with
-- | `canvasGame`
type CanvasGame state return =
  { canvas      :: QuerySelector
  , init        :: CanvasActionM state
  , update      :: Seconds -> state -> Effect state
  , display     :: state -> CanvasAction
  , end         :: state -> CanvasActionM (Maybe (Either Error return))
  , events      :: List (GameEvent state)
  }

-- | Make an `Aff` that will start the provided canvas game loop when run
canvasGame :: forall s a. CanvasGame s a -> Aff a
canvasGame g@{ init, update, events } = do
  ctx <- liftEffect getCtx >>= case _ of
    Nothing -> throwError (error "The canvas for canvasGame was not found.")
    Just ctx -> pure ctx
  game
    { init: g.init # runAction ctx
    , update
    , display: g.display >>> runAction ctx
    , end: g.end >>> runAction ctx
    , events
    }
    where
      toCanvasElement :: HTMLCanvasElement -> C.CanvasElement
      toCanvasElement = unsafeCoerce
      getCtx = do
        mCanv <- qSel g.canvas
          <#> (=<<) fromElement
          <#> map toCanvasElement
        for mCanv C.getContext2D

-- | Set the update field equal to this in a `Game`, `CanvasGame`, or
-- | `GameEvent` to make it do nothing. For a `Game` or `CanvasGame`, this means
-- | that the only way to modify the state is through events. For a `GameEvent`,
-- | it will make the event not do anything. This can be useful for events being
-- | passed to functions such as `inputEvent`, which does not use the `update`
-- | field.
noUpdate :: forall state e. e -> state -> Effect state
noUpdate = const pure

-- | Setting this as the `end` field in a `Game` or `CanvasGame` will cause it
-- | to keep going indefinitely, unless an error is thrown in an update function
-- | or similar.
noEnd
  :: forall m state return
   . Applicative m
  => state
  -> m (Maybe (Either Error return))
noEnd = const (pure Nothing)

-- | An empty list of events. Set this as the `events` field in a `Game` or
-- | `CanvasGame` if you're not using events.
noEvents :: forall state. List (GameEvent state)
noEvents = Nil

-- | An empty signal. Set this as the `signal` field in a `Game` or `CanvasGame`
-- | if you're not using signals.
-- noSignal = constant pure