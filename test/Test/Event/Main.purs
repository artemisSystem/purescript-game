module Test.Event.Main where

import Prelude

import Data.Foldable (oneOfMap)
import Effect (Effect)
import Effect.Class.Console (log)
import Game (mkReducer)
import Game.Aff (AffGame, _env, runGameEffect)
import Game.Aff.Web.Event (click)
import Game.Aff.Web.Util (qSel)
import Game.Util (asksAt)
import Game.Util.Maybe (runMaybe)
import Run (Run, EFFECT)
import Run.Except (FAIL)
import Run.State (modify, get)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.Element as Element
import Web.Event.EventTarget (EventTarget)


type Env =
  { button1 ∷ EventTarget
  , button2 ∷ EventTarget
  }

type Extra = ()

type State = Int

getButton ∷ ∀ r. String → Run (effect ∷ EFFECT, except ∷ FAIL | r) EventTarget
getButton button = qSel (QuerySelector button) <#> Element.toEventTarget

game ∷ AffGame Extra Env State Unit
game =
  { init: runMaybe "Buttons missing from HTML" ado
      button1 ← getButton "#button1"
      button2 ← getButton "#button2"
      in { env: { button1, button2 }, initState: 0 }
  , updates:
    [ click (oneOfMap (asksAt _env) [_.button1, _.button2]) do
        modify (_ + 1)
        count ← get
        log ("Total clicks: " <> show count)
    ]
  }

main ∷ Effect Unit
main = runGameEffect (mkReducer identity) game