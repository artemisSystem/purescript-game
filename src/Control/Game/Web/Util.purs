module Control.Game.Web.Util where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (QuerySelector, querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

-- | `querySelector` without having to supply a `ParentNode`, using the
-- | document as parent node.
qSel :: QuerySelector -> Effect (Maybe Element)
qSel sel = do
  doc <- window >>= document <#> toParentNode
  querySelector sel doc
