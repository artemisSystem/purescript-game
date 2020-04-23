module Game.Aff.Every where

import Prelude

import Data.Time.Duration (class Duration, fromDuration)
import Effect.Aff (Aff)
import Effect.Aff (delay) as Aff
import Game.Aff (AffGameUpdate, LoopExecIn, loopUpdate)
import Prim.Row (class Union)
import Run (AFF, Run, liftAff)
import Unsafe.Coerce (unsafeCoerce)


delayAff :: forall d. Duration d => d -> Aff Unit
delayAff d = Aff.delay (fromDuration d)

delay :: forall d r. Duration d => d -> Run (aff :: AFF | r) Unit
delay = liftAff <<< delayAff

everyUpdate
  :: forall extra update d s a
   . Duration d => Union (LoopExecIn s a) extra update
  => d -> Run update Unit -> AffGameUpdate extra s a
everyUpdate d update = loopUpdate (delay d) (coerce update)
  where
    coerce :: Run update Unit -> Run _ Unit
    coerce = unsafeCoerce

