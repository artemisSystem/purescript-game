module Control.Game.Util where

import Prelude

import Control.Monad.Loops (iterateUntilM)
import Data.Int (floor)
import Data.Maybe (Maybe)
import Data.Newtype (un)
import Data.Time.Duration (class Duration, Milliseconds(..), fromDuration, toDuration, Seconds)
import Data.DateTime.Instant (unInstant)
import Effect.Now (now)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write, modify, modify', modify_) as R
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (QuerySelector, querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

import Effect.Aff
import Effect.Class.Console

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

durationToInt :: forall d. Duration d => d -> Int
durationToInt = fromDuration >>> un Milliseconds >>> floor

nowSeconds :: Effect Seconds
nowSeconds = now <#> (unInstant >>> toDuration)

iterateM :: forall m a b. Monad m => (a -> m a) -> m a -> m b
iterateM f ma = ma >>= (f >>> iterateM f)

iterateUntilM'
  :: forall m a
   . Monad m
  => (a -> Boolean) -> (a -> m a) -> m a -> m a
iterateUntilM' p f ma = ma >>= iterateUntilM p f

-- | `querySelector` without having to supply a `ParentNode`, using the
-- | document as parent node.
qSel :: QuerySelector -> Effect (Maybe Element)
qSel sel = do
  doc <- window >>= document <#> toParentNode
  querySelector sel doc
