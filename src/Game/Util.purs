module Game.Util where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe, maybe)
import Data.Either (Either, either)
import Data.Newtype (un)
import Data.Time.Duration (class Duration, Seconds, fromDuration, toDuration)
import Effect.Now (now)
import Effect.Aff (Milliseconds(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write, modify, modify', modify_) as R


newRef :: forall m s. MonadEffect m => s -> m (Ref s)
newRef v = liftEffect (R.new v)

readRef :: forall m s. MonadEffect m => Ref s -> m s
readRef ref = liftEffect (R.read ref)

writeRef :: forall m s. MonadEffect m => s -> Ref s -> m Unit
writeRef v ref = liftEffect (R.write v ref)

modifyRef :: forall m s. MonadEffect m => (s -> s) -> Ref s -> m s
modifyRef f ref = liftEffect (R.modify f ref)

modifyRef'
  :: forall m s b
   . MonadEffect m
  => (s -> { state :: s, value :: b }) -> Ref s -> m b
modifyRef' f ref = liftEffect (R.modify' f ref)

modifyRef_ :: forall m s. MonadEffect m => (s -> s) -> Ref s -> m Unit
modifyRef_ f ref = liftEffect (R.modify_ f ref)

durationToNumber :: forall d. Duration d => d -> Number
durationToNumber = fromDuration >>> un Milliseconds

nowSeconds :: forall m. MonadEffect m => m Seconds
nowSeconds = liftEffect $ now <#> (unInstant >>> toDuration)

iterateM :: forall m a b. Monad m => (a -> m a) -> m a -> m b
iterateM f ma = ma >>= (f >>> iterateM f)

forever :: forall m a b. Monad m => m a -> m b
forever ma = iterateM (\_ -> ma) ma

fromLeft :: forall a. Either a Void -> a
fromLeft = either identity absurd

maybeThrow :: forall m a. MonadEffect m => String -> Maybe a -> m a
maybeThrow error = maybe (liftEffect $ throw error) pure
