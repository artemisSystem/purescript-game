module Control.Game
  (GameEvent
  , Game, game
  , CanvasGame, canvasGame
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.List (List)
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
import Web.DOM.ParentNode (QuerySelector, querySelector)
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (EventTarget)
import Web.HTML (window)
import Web.HTML.HTMLCanvasElement (fromElement, HTMLCanvasElement)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (cancelAnimationFrame, requestAnimationFrame, document)


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
  }

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
  , setupCanvas :: CanvasAction
  , init        :: Effect state
  , update      :: Seconds -> state -> Effect state
  , display     :: state -> CanvasAction
  , end         :: state -> CanvasActionM (Maybe (Either Error return))
  , events      :: List (GameEvent state)
  }

-- | Make an `Aff` that will start your canvas game loop when run
canvasGame :: forall s a. CanvasGame s a -> Aff a
canvasGame g@{ init, update, events } = do
  ctx <- liftEffect getCtx >>= case _ of
    Nothing -> throwError (error "The canvas for canvasGame was not found.")
    Just ctx -> pure ctx
  liftEffect do runAction ctx g.setupCanvas
  game
    { init
    , update
    , display: g.display >>> runAction ctx
    , end:     g.end >>> runAction ctx
    , events
    }
    where
      toCanvasElement :: HTMLCanvasElement -> C.CanvasElement
      toCanvasElement = unsafeCoerce
      getCtx = do
        doc <- window >>= document <#> toParentNode
        mCanv <- querySelector g.canvas doc
          <#> (=<<) fromElement
          <#> map toCanvasElement
        for mCanv C.getContext2D