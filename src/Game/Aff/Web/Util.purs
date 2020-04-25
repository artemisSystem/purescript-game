module Game.Aff.Web.Util where

import Prelude

import Data.Filterable (filterMap)
import Effect (Effect)
import Game.Util.Maybe (liftBoth)
import Run (EFFECT, Run, liftEffect)
import Run.Except (FAIL)
import Web.DOM.Element (Element, fromNode)
import Web.DOM.NodeList (NodeList, toArray)
import Web.DOM.ParentNode (QuerySelector, querySelector, querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

-- | `querySelector` without having to supply a `ParentNode`, using the
-- | document as parent node.
qSel
  :: forall r
   . QuerySelector
  -> Run (effect :: EFFECT, except :: FAIL | r) Element
qSel sel = liftBoth do
  doc <- window >>= document <#> toParentNode
  querySelector sel doc

-- | `querySelectorAll` without having to supply a `ParentNode`, using the
-- | document as parent node.
qSelAll :: forall r. QuerySelector -> Run (effect :: EFFECT | r) (Array Element)
qSelAll sel = liftEffect do
  doc <- window >>= document <#> toParentNode
  querySelectorAll sel doc >>= nodeListToElems

nodeListToElems :: NodeList -> Effect (Array Element)
nodeListToElems = toArray >>> map (filterMap fromNode)
