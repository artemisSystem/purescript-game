module Test.Signal.Main where

import Prelude

import Control.Game (canvasGame, noEnd, noEvents, noUpdate, noSignal)
import Data.Symbol (SProxy(..))
import Data.Vector.Polymorphic ((><))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Graphics.CanvasAction (clearRectFull, fillText, setFont)
import Record (modify)
import Signal.DOM (mouseButtonPressed, MouseButton(..))
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
  signal <- mouseButtonPressed MouseLeftButton <#>
    map case _ of
      true -> modify (SProxy :: _ "press") (add 1) >>> pure
      false -> modify (SProxy :: _ "release") (add 1) >>> pure
  launchAff_ $ canvasGame
    { canvas: QuerySelector "canvas"
    , init: { press: 0, release: 0 }
        <$ setFont "15px monospace"
    , update: noUpdate
    , display: \{ press, release } -> do
        clearRectFull
        fillText ("Presses: " <> show press) (10.0 >< 20.0)
        fillText ("Releases: " <> show release) (10.0 >< 40.0)
    , end: noEnd
    , events: noEvents
    -- Note: Here, i'm merging it with `noSignal` to prevent it from yielding
    -- a function which will increment the `release` field (which comes from
    -- `mouseButtonPressed`'s initial value `false`). Further explained in the
    -- documentation of `noSignal`.
    , signal: noSignal <> signal
    }
