module Control.Game.Web
  ( requestAnimationFrame
  , requestAnimationFrames
  , requestAnimationFramesUntil
  , AnimationFrameUpdate

  , requestAnimationFrame'
  , requestAnimationFrames'
  , requestAnimationFramesUntil'
  , AnimationFrameUpdate'

  , GameEvent
  ) where

import Prelude

import Control.Game
import Control.Game.Util
import Control.Monad.Loops (untilJust)
import Control.Monad.Rec.Class (forever)
import Data.Either
import Data.Maybe
import Data.Newtype
import Data.Time.Duration (Seconds(..))
import Data.Tuple
import Effect
import Effect.Aff
import Effect.Class
import Partial.Unsafe (unsafePartial)
import Web.Event.Event
import Web.Event.EventTarget
import Web.HTML (window) as W
import Web.HTML.Window (requestAnimationFrame, cancelAnimationFrame) as W
import Web.HTML.Window (Window)

-- TODO: `WebGame` and `CanvasGame`, which have `ToGame` instances
-- TODO: `CanvasUpdate`, similar to `EffectUpdate` (maybe, maybe not)


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

requestAnimationFramesUntil' :: forall a. (Seconds -> Effect (Maybe a)) -> Window -> Aff a
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
  , update :: Seconds -> EffectUpdate s a
  }

derive instance newtypeAnimationFrameUpdate' :: Newtype (AnimationFrameUpdate' s a) _

instance toUpdateAnimationFrameUpdate' :: ToUpdate s a (AnimationFrameUpdate' s a) where
  toUpdate (AnimationFrameUpdate' { window, update }) =
    \ref -> requestAnimationFramesUntil' (update >>> (_ `toEffect` ref)) window

requestAnimationFrame :: forall a. Effect a -> Aff a
requestAnimationFrame effect =
  liftEffect W.window >>= requestAnimationFrame' effect

requestAnimationFrames :: (Seconds -> Effect Unit) -> Aff Void
requestAnimationFrames effect =
  liftEffect W.window >>= requestAnimationFrames' effect

requestAnimationFramesUntil :: forall a. (Seconds -> Effect (Maybe a)) -> Aff a
requestAnimationFramesUntil effect =
  liftEffect W.window >>= requestAnimationFramesUntil' effect

newtype AnimationFrameUpdate s a =
  AnimationFrameUpdate (Seconds -> EffectUpdate s a)

derive instance newtypeAnimationFrameUpdate :: Newtype (AnimationFrameUpdate s a) _

instance toUpdateAnimationFrameUpdate :: ToUpdate s a (AnimationFrameUpdate s a) where
  toUpdate (AnimationFrameUpdate update) = \ref -> do
    window <- liftEffect W.window
    toUpdate (AnimationFrameUpdate' { window, update }) ref


newtype GameEvent s a = GameEvent
  { eventType  :: EventType
  , target     :: EventTarget
  , update     :: Event -> EffectUpdate s a
  , useCapture :: Boolean
  }

-- TODO: instance toUpdateGameEvent :: ToUpdate s a (GameEvent s a) where