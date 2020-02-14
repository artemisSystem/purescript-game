module Control.Game
  ( after
  , every
  , everyUntil
  , UpdateEvery

  , module Exports
  ) where

import Prelude

import Control.Game.Types (class ToUpdate, EffectUpdate, toEffect)
import Control.Game.Types (Game, class ToGame, runGame, class ToUpdate, toUpdate, addToGame, (:+), addMultipleToGame, (:*), EffectUpdate, toEffect) as Exports
import Control.Game.Util (durationToInt)
import Control.Monad.Loops (untilJust)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Time.Duration (class Duration)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Timer (clearTimeout, setTimeout)

after :: forall d a. Duration d => d -> Effect a -> Aff a
after d effect = makeAff \cb -> ado
  id <- setTimeout (durationToInt d) do
    a <- effect
    cb (Right a)
  in effectCanceler (clearTimeout id)

every :: forall d. Duration d => d -> Effect Unit -> Aff Void
every d effect = forever (after d effect)

everyUntil :: forall d a. Duration d => d -> Effect (Maybe a) -> Aff a
everyUntil d effect = untilJust (after d effect)


newtype UpdateEvery t s a = UpdateEvery
  { update   :: EffectUpdate s a
  , interval :: t
  }

derive instance newtypeUpdateEvery :: Newtype (UpdateEvery t s a) _

instance toUpdateUpdateEvery :: Duration t => ToUpdate s a (UpdateEvery t s a) where
  toUpdate (UpdateEvery { update, interval }) =
    \ref -> everyUntil interval (toEffect update ref)
