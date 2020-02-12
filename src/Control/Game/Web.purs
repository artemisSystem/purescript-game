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
import Control.Monad.Loops (untilJust)
import Control.Monad.Rec.Class (forever)
import Data.Either
import Data.Maybe
import Data.Newtype
import Effect
import Effect.Aff
import Effect.Class
import Web.Event.Event
import Web.Event.EventTarget
import Web.HTML (window) as W
import Web.HTML.Window (requestAnimationFrame, cancelAnimationFrame) as W
import Web.HTML.Window (Window)

-- TODO: `AnimGame` and `CanvasAnimGame`, which have `ToGame` instances
-- TODO: `CanvasUpdate`, similar to `EffectUpdate`

requestAnimationFrame' :: forall a. Effect a -> Window -> Aff a
requestAnimationFrame' effect w = makeAff \cb -> ado
  id <- w # W.requestAnimationFrame do
    a <- effect
    cb (Right a)
  in effectCanceler (W.cancelAnimationFrame id w)

requestAnimationFrames' :: Effect Unit -> Window -> Aff Void
requestAnimationFrames' effect w = forever (requestAnimationFrame' effect w)

requestAnimationFramesUntil' :: forall a. Effect (Maybe a) -> Window -> Aff a
requestAnimationFramesUntil' effect w =
  untilJust (requestAnimationFrame' effect w)

newtype AnimationFrameUpdate' s a = AnimationFrameUpdate'
  { window :: Window
  , update :: EffectUpdate s a
  }

derive instance newtypeAnimationFrameUpdate' :: Newtype (AnimationFrameUpdate' s a) _

instance toUpdateAnimationFrameUpdate' :: ToUpdate s a (AnimationFrameUpdate' s a) where
  toUpdate (AnimationFrameUpdate' { window, update }) =
    \ref -> requestAnimationFramesUntil' (toEffect update ref) window

requestAnimationFrame :: forall a. Effect a -> Aff a
requestAnimationFrame effect =
  liftEffect W.window >>= requestAnimationFrame' effect

requestAnimationFrames :: Effect Unit -> Aff Void
requestAnimationFrames effect =
  liftEffect W.window >>= requestAnimationFrames' effect

requestAnimationFramesUntil :: forall a. Effect (Maybe a) -> Aff a
requestAnimationFramesUntil effect =
  liftEffect W.window >>= requestAnimationFramesUntil' effect

newtype AnimationFrameUpdate s a = AnimationFrameUpdate (EffectUpdate s a)

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