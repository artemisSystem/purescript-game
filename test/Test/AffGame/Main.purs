-- | A simple animation of a rectangle expanding to cover the canvas
module Test.AffGame.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Vector.Polymorphic (Rect(..), Vector2, makeRect, (><))
import Effect (Effect)
import Effect.Class.Console (log)
import Game (mkReducer)
import Game.Aff (AffGame, FPS(..), runGameEffect, _env)
import Game.Aff.Every (everyUpdate)
import Game.Aff.Web (animationFrameUpdate, animationFrameMatchInterval)
import Game.Util (nowSeconds, maybeThrow)
import Graphics.CanvasAction (CanvasPattern, PatternRepeat(..), QuerySelector(..), clearRectFull, createPattern, fillRect, fillRectFull, filled, imageSource, querySelectContext2D, runActionOffscreen)
import Graphics.CanvasAction.Run (CANVAS, runCanvas)
import Run.State (get, modify)
import Run.Reader (askAt)


type Extra = (canvas :: CANVAS)

type Env = CanvasPattern

type State = Vector2 Number

game :: AffGame Extra Env State Unit
game =
  { init: ado
      pattern <- runActionOffscreen (20.0 >< 20.0) do
        filled "#aaf" fillRectFull
        filled "#afa" $ fillRect (makeRect 0.0 10.0 10.0 10.0)
        filled "#faa" $ fillRect (makeRect 10.0 0.0 10.0 10.0)
        imageSource >>= (_ `createPattern` Repeat)
      in Tuple pattern (0.0 >< 0.0)
  , updates:
    [ animationFrameUpdate do
        pattern <- askAt _env
        clearRectFull
        dimensions <- get
        filled pattern $ fillRect (Rect zero dimensions)
    , everyUpdate (Milliseconds 100.0) do
        modify ((add 3.0 >< add 1.0) <*> _)
    , animationFrameMatchInterval (FPS 2.0) do
        modify ((identity >< add 12.0) <*> _)
        (Seconds now) <- nowSeconds
        log ("Now: " <> show now)
    ]
  }


main :: Effect Unit
main = do
  ctx <- querySelectContext2D (QuerySelector "canvas#game")
     >>= maybeThrow "no canvas"
  runGameEffect (mkReducer do runCanvas ctx) game