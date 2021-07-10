module Game.Util where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Either (Either, either)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (un)
import Data.Time.Duration (class Duration, Seconds, fromDuration, toDuration)
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write, modify, modify', modify_) as R
import Run (Run, EFFECT)
import Run.State (STATE, runState)
import Type.Row (type (+))



newRef ∷ ∀ m s. MonadEffect m ⇒ s → m (Ref s)
newRef v = liftEffect (R.new v)

readRef ∷ ∀ m s. MonadEffect m ⇒ Ref s → m s
readRef ref = liftEffect (R.read ref)

writeRef ∷ ∀ m s. MonadEffect m ⇒ s → Ref s → m Unit
writeRef v ref = liftEffect (R.write v ref)

modifyRef ∷ ∀ m s. MonadEffect m ⇒ (s → s) → Ref s → m s
modifyRef f ref = liftEffect (R.modify f ref)

modifyRef' ∷
  ∀ m s b. MonadEffect m ⇒ (s → { state ∷ s, value ∷ b }) → Ref s → m b
modifyRef' f ref = liftEffect (R.modify' f ref)

modifyRef_ ∷ ∀ m s. MonadEffect m ⇒ (s → s) → Ref s → m Unit
modifyRef_ f ref = liftEffect (R.modify_ f ref)

durationToNumber ∷ ∀ d. Duration d ⇒ d → Number
durationToNumber = fromDuration >>> un Milliseconds

nowSeconds ∷ ∀ m. MonadEffect m ⇒ m Seconds
nowSeconds = liftEffect $ now <#> (unInstant >>> toDuration)

-- todo: change this to not take a monadic value as initializer
iterateM ∷ ∀ m a b. Monad m ⇒ (a → m a) → m a → m b
iterateM f ma = ma >>= (f >>> iterateM f)

forever ∷ ∀ m a b. Monad m ⇒ m a → m b
forever ma = iterateM (\_ → ma) ma

fromLeft ∷ ∀ a. Either a Void → a
fromLeft = either identity absurd

maybeThrow ∷ ∀ m a. MonadEffect m ⇒ String → Maybe a → m a
maybeThrow error = maybe (liftEffect $ throw error) pure

runStateWithRef ∷ ∀ r s. Ref s → Run (EFFECT + STATE s + r) ~> Run (EFFECT + r)
runStateWithRef ref run = do
  init ← readRef ref
  (Tuple s a) ← runState init run
  writeRef s ref
  pure a
