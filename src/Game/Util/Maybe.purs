module Game.Util.Maybe
  ( liftMaybe
  , liftMaybeF
  , liftEffectF
  , liftBoth
  , liftBothF
  , runMaybe
  , runMaybe'
  , module Exports
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Game.Util (maybeThrow)
import Run (EFFECT, Run, liftEffect, runBaseEffect)
import Run (liftEffect) as Exports
import Run.Except (FAIL, fail, runFail)
import Type.Row (type (+))


-- todo: repurpose this module to allow for easy noting of `Maybe a` and
-- `Effect (Maybe a)`s into `Run (EFFECT + EXCEPT e + r) a`.
-- Re-export `note` from `Run.Except`
liftMaybe ∷ ∀ r a. Maybe a → Run (FAIL + r) a
liftMaybe (Just a) = pure a
liftMaybe Nothing = fail

liftMaybeF ∷ ∀ f r a. Functor f ⇒ f (Maybe a) → f (Run (FAIL + r) a)
liftMaybeF = map liftMaybe

liftEffectF ∷ ∀ f r a. Functor f ⇒ f (Effect a) → f (Run (EFFECT + r) a)
liftEffectF = map liftEffect

liftBoth ∷ ∀ r a. Effect (Maybe a) → Run (EFFECT + FAIL + r) a
liftBoth = liftEffect >=> liftMaybe

liftBothF ∷
  ∀ f r a. Functor f ⇒ f (Effect (Maybe a)) → f (Run (EFFECT + FAIL + r) a)
liftBothF = map liftBoth

runMaybe ∷ ∀ r a. String → Run (EFFECT + FAIL + r) a → Run (EFFECT + r) a
runMaybe errorMsg = runFail >=> maybeThrow errorMsg

runMaybe' ∷ ∀ a. String → Run (EFFECT + FAIL + ()) a → Effect a
runMaybe' msg = runBaseEffect <<< runMaybe msg
