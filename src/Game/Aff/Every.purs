module Game.Aff.Every where

import Prelude

import Data.Time.Duration (class Duration, fromDuration)
import Effect.Aff (Aff)
import Effect.Aff (delay) as Aff
import Game.Aff (AffGameUpdate, LoopExecIn, loopUpdate')
import Run (Run)


delay ∷ ∀ d. Duration d ⇒ d → Aff Unit
delay d = Aff.delay (fromDuration d)

-- | An `AffGameUpdate` that runs its update at the specified interval
everyUpdate ∷
  ∀ extra e s a d
  . Duration d
  ⇒ d → Run (LoopExecIn e s a extra) Unit → AffGameUpdate extra e s a
everyUpdate d = loopUpdate' (delay d)
