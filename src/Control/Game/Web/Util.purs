module Control.Game.Web.Util where

import Prelude

import Data.Filterable (filterMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import Web.DOM.Element (Element, fromNode)
import Web.DOM.NodeList (NodeList, toArray)
import Web.DOM.ParentNode (QuerySelector, querySelector, querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

-- | `querySelector` without having to supply a `ParentNode`, using the
-- | document as parent node.
qSel :: QuerySelector -> Effect (Maybe Element)
qSel sel = do
  doc <- window >>= document <#> toParentNode
  querySelector sel doc

-- | `querySelectorAll` without having to supply a `ParentNode`, using the
-- | document as parent node.
qSelAll :: QuerySelector -> Effect (Array Element)
qSelAll sel = do
  doc <- window >>= document <#> toParentNode
  querySelectorAll sel doc >>= nodeListToElems

nodeListToElems :: NodeList -> Effect (Array Element)
nodeListToElems = toArray >>> map (filterMap fromNode)
