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
  ∀ extra env state err a
  . Window
  → Run (LoopExecIn env state err a extra) Unit
  → AffGameUpdate extra env state err a
animationFrameUpdate' w update = loopUpdate' (delayFrame' w) update


delayFrame ∷ Aff Unit
delayFrame = liftEffect W.window >>= delayFrame'

animationFrameUpdate ∷
  ∀ extra env state err a
  . Run (LoopExecIn env state err a extra) Unit
  → AffGameUpdate extra env state err a
animationFrameUpdate = loopUpdate' delayFrame

-- | Uses `requestAnimationFrame` to run the given update at the given interval
-- | (`d`) as accurately as possible
animationFrameMatchInterval ∷
  ∀ extra env state err a d
  . Duration d
  ⇒ Run (LoopExecIn env state err a extra) d
  → Run (LoopExecIn env state err a extra) Unit
  → AffGameUpdate extra env state err a
animationFrameMatchInterval = matchInterval delayFrame
