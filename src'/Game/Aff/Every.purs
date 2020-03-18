module Game.Aff.Every where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (class Duration)
import Effect.Aff (effectCanceler, makeAff)
import Effect.Timer (clearTimeout, setTimeout)
import Game (GameUpdate)
import Game.Aff (GameEffects, Looper, loopAction, loopUpdate)
import Game.Util (durationToInt)
import Run (AFF, EFFECT, Run, liftAff, runBaseEffect)


-- | After `d` has passed, run the effect and resolve an `Aff` with its result
after
  :: forall r d a
   . Duration d
  => d -> Run (effect :: EFFECT) a -> Run (effect :: EFFECT, aff :: AFF | r) a
after d effect = liftAff $ makeAff \cb -> ado
  id <- setTimeout (durationToInt d) do
    a <- runBaseEffect effect
    cb (Right a)
  in effectCanceler (clearTimeout id)

every
  :: forall d s a
   . Duration d
  => d -> Run (GameEffects s a) Unit -> Run (Looper s) a
every d = loopAction (after d)

updateEvery
  :: forall d r s a
   . Duration d
  => d -> Run r Unit -> GameUpdate (GameEffects s a) r (Looper s) a
updateEvery d = loopUpdate (after d)