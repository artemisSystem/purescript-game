module Control.Game.Every where

import Prelude

import Control.Game (GameEffects, _end, GameUpdate)
import Control.Game.Util (durationToInt, readRef, writeRef)
import Control.Monad.Loops (untilJust)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (class Duration)
import Data.Tuple (fst)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Ref (Ref)
import Effect.Timer (clearTimeout, setTimeout)
import Run (EFFECT, Run, runBaseEffect)
import Run.Except (runExceptAt)
import Run.State (runState)

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
every d run stateRef = untilJust $ after d do
  state <- readRef stateRef
  result <- run
    # runState state >>> map fst
    # runExceptAt _end
  case result of
    Left a -> pure (Just a)
    Right s -> writeRef s stateRef $> Nothing

updateEvery
  :: forall d r s a
   . Duration d
  => d -> Run r Unit -> GameUpdate (GameEffects s a) r s a
updateEvery d update =
  { update
  , loop: every d
  }