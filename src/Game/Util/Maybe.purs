module Game.Util.Maybe
  ( liftMaybe
  , liftMaybeF
  , liftEffectF
  , liftBoth
  , liftBothF
  , runMaybe
  , module Exports
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Game.Util (maybeThrow)
import Run (liftEffect) as Exports
import Run (EFFECT, Run, liftEffect, runBaseEffect)
import Run.Except (FAIL, fail, runFail)

liftMaybe :: forall r a. Maybe a -> Run (except :: FAIL | r) a
liftMaybe (Just a) = pure a
liftMaybe Nothing = fail

liftMaybeF
  :: forall f r a
   . Functor f
  => f (Maybe a) -> f (Run (except :: FAIL | r) a)
liftMaybeF = map liftMaybe

liftEffectF
  :: forall f r a
   . Functor f
  => f (Effect a) -> f (Run (effect :: EFFECT | r) a)
liftEffectF = map liftEffect

liftBoth
  :: forall r a
   . Effect (Maybe a)
  -> Run (effect :: EFFECT, except :: FAIL | r) a
liftBoth = liftEffect >=> liftMaybe

liftBothF
  :: forall f r a
   . Functor f
  => f (Effect (Maybe a))
  -> f (Run (effect :: EFFECT, except :: FAIL | r) a)
liftBothF = map liftBoth

runMaybe
  :: forall r a
   . String
  -> Run (effect :: EFFECT, except :: FAIL | r) a
  -> Run (effect :: EFFECT | r) a
runMaybe errorMsg = runFail >=> maybeThrow errorMsg

runMaybe'
  :: forall a
   . String
  -> Run (effect :: EFFECT, except :: FAIL) a
  -> Effect a
runMaybe' msg = runBaseEffect <<< runMaybe msg
