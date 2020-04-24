-- | A simple animation of a rectangle expanding to cover the canvas
module Test.AffGame.Main where

import Prelude

import Data.Maybe (maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Vector.Polymorphic (makeRect, (><))
import Effect (Effect)
import Effect.Exception (throw)
import Game (mkReducer)
import Game.Aff (AffGame, runGameEffect)
import Game.Aff.Every (everyUpdate)
import Game.Aff.Web (animationFrameUpdate)
import Graphics.CanvasAction (CanvasPattern, PatternRepeat(..), QuerySelector(..), clearRectFull, createPattern, fillRect, fillRectFull, filled, imageSource, querySelectContext2D, runActionOffscreen)
import Graphics.CanvasAction.Run (CANVAS, runCanvas)
import Run.State (get, modify)


type Extra = (canvas :: CANVAS)

type State = { x :: Number, y :: Number }

game :: CanvasPattern -> AffGame Extra State Unit
game pattern =
  [ animationFrameUpdate do
      clearRectFull
      { x, y } <- get
      filled pattern $ fillRect (makeRect 0.0 0.0 x y)
  , everyUpdate (Milliseconds 100.0) do
      modify \{ x, y } -> { x: x + 1.0, y: y + 1.0 }
  ]

main :: Effect Unit
main = do
  pattern <- runActionOffscreen (20.0 >< 20.0) do
    filled "#aaf" fillRectFull
    filled "#afa" $ fillRect (makeRect 0.0 10.0 10.0 10.0)
    filled "#faa" $ fillRect (makeRect 10.0 0.0 10.0 10.0)
    imageSource >>= (_ `createPattern` Repeat)
  ctx <- querySelectContext2D (QuerySelector "canvas#game")
     >>= maybe (throw "no canvas") pure
  runGameEffect { x: 0.0, y: 0.0 } (mkReducer do runCanvas ctx) (game pattern)