module Game.Aff.Web where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Ref (Ref)
import Game
import Game.Aff
import Run
import Web.HTML (window) as W
import Web.HTML.Window (requestAnimationFrame, cancelAnimationFrame) as W
import Web.HTML.Window (Window)


requestAnimationFrame'
  :: forall a
   . Window -> Run (effect :: EFFECT) a -> Run (effect :: EFFECT, aff :: AFF) a
requestAnimationFrame' w effect = liftAff $ makeAff \cb -> ado
  id <- w # W.requestAnimationFrame do
    a <- runBaseEffect effect
    cb (Right a)
  in effectCanceler (W.cancelAnimationFrame id w)

requestAnimationFrames'
  :: forall s a
   . Window
  -> Run (GameEffects s a) Unit
  -> Run (Looper s) a
requestAnimationFrames' w = loopAction (requestAnimationFrame' w)

animationFrameUpdate'
  :: forall r s a
   . Window -> Run r Unit -> GameUpdate (GameEffects s a) r (Looper s) a
animationFrameUpdate' w = loopUpdate (requestAnimationFrame' w)


requestAnimationFrame
  :: forall a. Run (effect :: EFFECT) a -> Run (effect :: EFFECT, aff :: AFF) a
requestAnimationFrame effect =
  liftEffect W.window >>= \w -> requestAnimationFrame' w effect

requestAnimationFrames
  :: forall s a. Run (GameEffects s a) Unit -> Run (Looper s) a
requestAnimationFrames effect =
  liftEffect W.window >>= \w -> requestAnimationFrames' w effect

animationFrameUpdate
  :: forall r s a. Run r Unit -> GameUpdate (GameEffects s a) r (Looper s) a
animationFrameUpdate update =
  { update
  , loop: \run -> liftEffect W.window >>= \w ->
      requestAnimationFrames' w run
  }

type GameEvent r s a =
  { update     :: Event -> Ref s -> m (Maybe a)
  , eventType  :: EventType
  , target     :: EventTarget
  , useCapture :: Boolean
  }