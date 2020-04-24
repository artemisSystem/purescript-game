module Game.Aff.Every where

import Prelude

import Data.Time.Duration (class Duration, fromDuration)
import Effect.Aff (Aff)
import Effect.Aff (delay) as Aff
import Game.Aff (AffGameUpdate, LoopExecIn, loopUpdate')
import Run (AFF, Run, liftAff)


delayAff :: forall d. Duration d => d -> Aff Unit
delayAff d = Aff.delay (fromDuration d)

delay :: forall d r. Duration d => d -> Run (aff :: AFF | r) Unit
delay = liftAff <<< delayAff

everyUpdate
  :: forall extra d s a
   . Duration d
  => d -> Run (LoopExecIn s a extra) Unit -> AffGameUpdate extra s a
everyUpdate d = loopUpdate' (delay d)

