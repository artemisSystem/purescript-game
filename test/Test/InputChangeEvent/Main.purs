module Test.InputChangeEvent.Main where

import Prelude

import Control.Game (canvasGame)
import Control.Game.EventHelpers (inputChangeEventValue)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Vector.Polymorphic ((><))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Graphics.CanvasAction (fillText, setFont, clearRectFull, filled, fillRectFull)
import Web.DOM.ParentNode (QuerySelector(..))

-- | In this example I'm using `Vector2` as the `Traversable t` in
-- | `inputChangeEventValue`'s type signature, because it's a simple
-- | `Traversable` that I can easily pattern match on, and I already use it as
-- | the `ToPos` I need to position text. There is also an end condition, which
-- | succeeds when the two fields say "stop" and "loop". The return value is
-- | `unit`, but it's not important here since it's thrown away because
-- | `launchAff_` is used instead of for example `launchAff` or `runAff`.
main :: Effect Unit
main = do
  events <- inputChangeEventValue
    do QuerySelector <$> ("input#a" >< "input#b")
    do \(a >< b) _ -> pure { a, b }
  launchAff_ $ canvasGame
    { canvas: QuerySelector "canvas"
    , setupCanvas: setFont "15px monospace"
    , init: pure { a: "", b: "" }
    , update: const pure
    , display: \{ a, b } -> do
        clearRectFull
        fillText a (10.0 >< 20.0)
        fillText b (10.0 >< 40.0)
    , end: case _ of
        { a: "stop", b: "loop" } -> ado
          filled "black" fillRectFull
          in Just (Right unit)
        _ -> pure Nothing
    , events
    }