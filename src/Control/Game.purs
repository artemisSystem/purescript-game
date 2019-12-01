module Control.Game (GameEvent, Game, game) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, Error, effectCanceler, makeAff)
import Effect.Now (now)
import Effect.Ref (new, read, write) as R
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (EventTarget)
import Web.HTML.Window (Window, cancelAnimationFrame, requestAnimationFrame)


-- | The type of events that can be used in `Game`
type GameEvent state =
  { eventType  :: EventType
  , target     :: EventTarget
  , update     :: Event -> state -> Effect state
  , useCapture :: Boolean
  }

-- | The type of a game that can be run with `game`
type Game state return =
  { gameWindow :: Window
  , init       :: Effect state
  , update     :: Seconds -> state -> Effect state
  , display    :: state -> Effect Unit
  , end        :: state -> Maybe (Either Error return)
  , events     :: List (GameEvent state)
  }

-- | Make an `Aff` that will start your game loop when run
game :: forall s a. Game s a -> Aff a
game { init, update, display, end, events, gameWindow } = makeAff \cb -> do
  -- Mutable values
  state <- init >>= R.new
  rafID <- R.new Nothing
  -- Setup events
  let getListener { update: evtUpdate } = eventListener \event -> do
        newState <- R.read state >>= (\old -> evtUpdate event old)
        R.write newState state
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
      for_ id (\id' -> cancelAnimationFrame id' gameWindow)
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
      case end newState of
        Just ea -> do
          cancel
          cb ea
        Nothing -> do
          id <- requestAnimationFrame (tick t) gameWindow
          R.write (Just id) rafID
  Milliseconds t0 <- unInstant <$> now
  -- Initial requestAnimationFrame
  id0 <- requestAnimationFrame (tick t0) gameWindow
  R.write (Just id0) rafID
  -- Canceler
  pure (effectCanceler cancel)