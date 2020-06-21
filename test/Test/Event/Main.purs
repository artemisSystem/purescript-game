module Test.Event.Main where

import Prelude

import Data.Foldable (oneOfMap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Game (mkReducer)
import Game.Aff (AffGame, _env, runGameEffect)
import Game.Aff.Web.Event (click)
import Game.Aff.Web.Util (qSel)
import Game.Util (asksAt)
import Game.Util.Maybe (liftMaybeF, runMaybe)
import Run.State (modify, get)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLButtonElement (HTMLButtonElement)
import Web.HTML.HTMLButtonElement as Button

type Env =
  { button1 ∷ HTMLButtonElement
  , button2 ∷ HTMLButtonElement
  }

type Extra = ()

type State = Int

game ∷ AffGame Extra Env State Unit
game =
  { init: runMaybe "Buttons missing" ado
      button1 ← qSel (QuerySelector "#button1") >>= liftMaybeF Button.fromElement
      button2 ← qSel (QuerySelector "#button2") >>= liftMaybeF Button.fromElement
      in Tuple { button1, button2 } 0
  , updates:
    [ click
        do oneOfMap (asksAt _env >>> map Button.toEventTarget)
             [_.button1, _.button2]
        do
          modify (_ + 1)
          count ← get
          log ("Total clicks: " <> show count)
    ]
  }

main ∷ Effect Unit
main = runGameEffect (mkReducer identity) game