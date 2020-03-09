module Game.Web where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Game (GameEffects, GameUpdate, loopAction, loopUpdate)
import Run (Run, EFFECT, runBaseEffect)
import Web.HTML (window) as W
import Web.HTML.Window (requestAnimationFrame, cancelAnimationFrame) as W
import Web.HTML.Window (Window)


requestAnimationFrame' :: forall a. Window -> Run (effect :: EFFECT) a -> Aff a
requestAnimationFrame' w effect = makeAff \cb -> ado
  id <- w # W.requestAnimationFrame do
    a <- runBaseEffect effect
    cb (Right a)
  in effectCanceler (W.cancelAnimationFrame id w)

requestAnimationFrames'
  :: forall s a
   . Window
  -> Run (GameEffects s a) Unit
  -> Ref s
  -> Aff a
requestAnimationFrames' w = loopAction (requestAnimationFrame' w)

animationFrameUpdate'
  :: forall r s a. Window -> Run r Unit -> GameUpdate (GameEffects s a) r s a
animationFrameUpdate' w = loopUpdate (requestAnimationFrame' w)


-- | Returns an `Aff` that runs the given effect and resolves with its result on
-- | the next animation frame
requestAnimationFrame :: forall a. Run (effect :: EFFECT) a -> Aff a
requestAnimationFrame effect =
  liftEffect W.window >>= \w -> requestAnimationFrame' w effect

-- | Returns an `Aff` that runs forever unless cancelled, and calls the provided
-- | effect with the time since the last animation frame in seconds every
-- | animation frame
requestAnimationFrames
  :: forall s a
   . Run (GameEffects s a) Unit
  -> Ref s
  -> Aff a
requestAnimationFrames effect stateRef =
  liftEffect W.window >>= \w -> requestAnimationFrames' w effect stateRef

animationFrameUpdate
  :: forall r s a. Run r Unit -> GameUpdate (GameEffects s a) r s a
animationFrameUpdate update =
  { update
  , loop: \run ref -> liftEffect W.window >>= \w ->
      requestAnimationFrames' w run ref
  }

-- type GameEvent r s a =
--   { eventType  :: EventType
--   , target     :: EventTarget
--   , update     :: Event -> Ref s -> m (Maybe a)
--   , useCapture :: Boolean
--   }