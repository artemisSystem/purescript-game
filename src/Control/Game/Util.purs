module Control.Game.Util where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write, modify, modify', modify_) as R
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (QuerySelector, querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)


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

-- | `querySelector` without having to supply a `ParentNode`, using the
-- | document as parent node.
qSel :: QuerySelector -> Effect (Maybe Element)
qSel sel = do
  doc <- window >>= document <#> toParentNode
  querySelector sel doc