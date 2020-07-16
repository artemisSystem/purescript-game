module Game.Aff.AnimationFrame where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (class Duration)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Game.Aff (AffGameUpdate, LoopExecIn, loopUpdate', matchInterval)
import Run (Run)
import Web.HTML (window) as W
import Web.HTML.Window (requestAnimationFrame, cancelAnimationFrame) as W
import Web.HTML.Window (Window)

delayFrame' ∷ Window → Aff Unit
delayFrame' w = makeAff \cb → do
  id ← W.requestAnimationFrame (cb (Right unit)) w
  pure $ effectCanceler do
    W.cancelAnimationFrame id w

animationFrameUpdate' ∷
  ∀ extra e s a
  . Window → Run (LoopExecIn e s a extra) Unit → AffGameUpdate extra e s a
animationFrameUpdate' w update = loopUpdate' (delayFrame' w) update


delayFrame ∷ Aff Unit
delayFrame = liftEffect W.window >>= delayFrame'

animationFrameUpdate ∷
  ∀ extra e s a
  . Run (LoopExecIn e s a extra) Unit → AffGameUpdate extra e s a
animationFrameUpdate = loopUpdate' delayFrame

animationFrameMatchInterval ∷
  ∀ extra e s a d
  . Duration d
  ⇒ Run (LoopExecIn e s a extra) d
  → Run (LoopExecIn e s a extra) Unit
  → AffGameUpdate extra e s a
animationFrameMatchInterval = matchInterval delayFrame
