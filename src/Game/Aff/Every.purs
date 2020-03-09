module Game.Aff.Every where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (class Duration)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Ref (Ref)
import Effect.Timer (clearTimeout, setTimeout)
import Game (GameEffects, GameUpdate, loopAction, loopUpdate)
import Game.Util (durationToInt)
import Run (EFFECT, Run, runBaseEffect)


-- | After `d` has passed, run the effect and resolve an `Aff` with its result
after :: forall d a. Duration d => d -> Run (effect :: EFFECT) a -> Aff a
after d effect = makeAff \cb -> ado
  id <- setTimeout (durationToInt d) do
    a <- runBaseEffect effect
    cb (Right a)
  in effectCanceler (clearTimeout id)

every
  :: forall d s a
   . Duration d
  => d -> Run (GameEffects s a) Unit -> Ref s -> Aff a
every d = loopAction (after d)

updateEvery
  :: forall d r s a
   . Duration d
  => d -> Run r Unit -> GameUpdate (GameEffects s a) r s a
updateEvery d = loopUpdate (after d)