module Game.Aff.Every where

import Prelude

import Data.Time.Duration (class Duration, fromDuration)
import Effect.Aff (Aff)
import Effect.Aff (delay) as Aff
import Game.Aff (AffGameUpdate, LoopExecIn, loopUpdate')
import Run (AFF, Run, liftAff)


delayAff ∷ ∀ d. Duration d ⇒ d → Aff Unit
delayAff d = Aff.delay (fromDuration d)

delay ∷ ∀ d r. Duration d ⇒ d → Run (aff ∷ AFF | r) Unit
delay = liftAff <<< delayAff

everyUpdate ∷
  ∀ extra e s a d
  . Duration d
  ⇒ d → Run (LoopExecIn e s a extra) Unit → AffGameUpdate extra e s a
everyUpdate d = loopUpdate' (delay d)
