module Control.Game.Web
  ( requestAnimationFrame
  , requestAnimationFrames
  , requestAnimationFramesUntil
  , AnimationFrameUpdate(..)

  , requestAnimationFrame'
  , requestAnimationFrames'
  , requestAnimationFramesUntil'
  , AnimationFrameUpdate'(..)

  , GameEvent(..)
  , hoistGameEvent
  , WebGame(..)
  ) where

import Prelude

import Control.Game (class ToGame, class ToUpdate, Game(..), toUpdate, (:*), (:+))
import Control.Game.Util (iterateM, iterateUntilM', newRef, nowSeconds, readRef, writeRef)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (class Newtype, over, over2)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Partial.Unsafe (unsafePartial)
import Record (modify)
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener, removeEventListener)
import Web.HTML (window) as W
import Web.HTML.Window (requestAnimationFrame, cancelAnimationFrame) as W
import Web.HTML.Window (Window)


requestAnimationFrame' :: forall a. Effect a -> Window -> Aff a
requestAnimationFrame' effect w = makeAff \cb -> ado
  id <- w # W.requestAnimationFrame do
    a <- effect
    cb (Right a)
  in effectCanceler (W.cancelAnimationFrame id w)

requestAnimationFrames' :: (Seconds -> Effect Unit) -> Window -> Aff Void
requestAnimationFrames' effect w = iterateM
  do \t0 -> requestAnimationFrame' (step t0) w
  do liftEffect nowSeconds
  where
    step t0 = do
      t <- nowSeconds
      effect (over2 Seconds (-) t t0) $> t

requestAnimationFramesUntil'
  :: forall a
   . (Seconds -> Effect (Maybe a))
  -> Window
  -> Aff a
requestAnimationFramesUntil' effect w = fixReturn $ iterateUntilM'
  do \(Tuple _ m) -> isJust m
  do \(Tuple t0 _) -> requestAnimationFrame' (step t0) w
  do liftEffect nowSeconds <#> (_ `Tuple` Nothing)
  where
    step t0 = do
      t <- nowSeconds
      effect (over2 Seconds (-) t t0) <#> Tuple t
    fixReturn = map (snd >>> unsafePartial fromJust)

newtype AnimationFrameUpdate' s a = AnimationFrameUpdate'
  { window :: Window
  , update :: Seconds -> Ref s -> Effect (Maybe a)
  }

derive instance newtypeAnimationFrameUpdate' :: Newtype (AnimationFrameUpdate' s a) _

instance toUpdateAnimationFrameUpdate' :: ToUpdate s a (AnimationFrameUpdate' s a) where
  toUpdate (AnimationFrameUpdate' { window, update }) =
    \ref -> requestAnimationFramesUntil' (update >>> (_ $ ref)) window


-- | Returns an `Aff` that runs the given effect and resolves with its result on
-- | the next animation frame
requestAnimationFrame :: forall a. Effect a -> Aff a
requestAnimationFrame effect =
  liftEffect W.window >>= requestAnimationFrame' effect

-- | Returns an `Aff` that runs forever unless cancelled, and calls the provided
-- | effect with the time since the last animation frame in seconds every
-- | animation frame
requestAnimationFrames :: (Seconds -> Effect Unit) -> Aff Void
requestAnimationFrames effect =
  liftEffect W.window >>= requestAnimationFrames' effect

-- | Returns an `Aff` that calls the provided effect with the time since the
-- | previous animation frame. When the effect returns a `Just`, it resolves
-- | with the contained value.
requestAnimationFramesUntil :: forall a. (Seconds -> Effect (Maybe a)) -> Aff a
requestAnimationFramesUntil effect =
  liftEffect W.window >>= requestAnimationFramesUntil' effect

-- get ref every frame or potentially call unnecesscary pure and pure unit every frame?
-- which is worse, which is preferrable?
-- won't be getting ref every frame anyway
newtype AnimationFrameUpdate s a =
  AnimationFrameUpdate (Seconds -> Ref s -> Effect (Maybe a))

derive instance newtypeAnimationFrameUpdate :: Newtype (AnimationFrameUpdate s a) _

instance toUpdateAnimationFrameUpdate :: ToUpdate s a (AnimationFrameUpdate s a) where
  toUpdate (AnimationFrameUpdate update) = \ref -> do
    window <- liftEffect W.window
    toUpdate (AnimationFrameUpdate' { window, update }) ref

-- maybe not have this parametrized by m anyway
-- have an effect update class, which are things that need to be convertable to (Ref s -> Effect (Maybe a))
newtype GameEvent m s a = GameEvent
  { eventType  :: EventType
  , target     :: EventTarget
  , update     :: Event -> Ref s -> m (Maybe a)
  , useCapture :: Boolean
  }

derive instance newTypeGameEvent :: Newtype (GameEvent m s a) _

instance toUpdateGameEventEffect :: ToUpdate s a (GameEvent Effect s a) where
  toUpdate (GameEvent { eventType, target, update, useCapture }) =
    \ref -> makeAff \cb -> do
      listenerRef <- newRef Nothing
      let canceler = readRef listenerRef >>= traverse_ \l ->
            removeEventListener eventType l useCapture target
      listener <- eventListener \event ->
        update event ref >>= case _ of
          Just a -> canceler *> cb (Right a)
          Nothing -> pure unit
      writeRef (Just listener) listenerRef
      addEventListener eventType listener useCapture target
      pure $ effectCanceler canceler

hoistGameEvent
  :: forall m s a
   . (m ~> Effect)
  -> GameEvent m s a
  -> GameEvent Effect s a
hoistGameEvent nat = over GameEvent
  $ modify (SProxy :: _ "update") (map <<< map $ nat)


newtype WebGame s a = WebGame
  { init   :: Aff s
  , frames :: Seconds -> Ref s -> Effect (Maybe a)
  , events :: Array (GameEvent Effect s a)
  }

derive instance newtypeWebGame :: Newtype (WebGame s a) _

instance toGameWebGame :: ToGame s a (WebGame s a) where
  toGame (WebGame { init, frames, events }) = pure $
    Game { init, update: [] }
      :+ do flip frames <#> requestAnimationFramesUntil
      :* events