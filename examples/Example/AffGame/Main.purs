-- | A simple animation of a rectangle expanding to cover the canvas. The width
-- | and height change in different ways.
module Example.AffGame.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Vector.Polymorphic (Rect(..), Vector2, makeRect, (><))
import Effect (Effect)
import Effect.Class.Console (log)
import Game (mkReducer)
import Game.Aff (AffGame, FPS(..), _env, mkAffGame, onStart, launchGame_)
import Game.Aff.AnimationFrame (animationFrameUpdate, animationFrameMatchInterval)
import Game.Aff.Every (everyUpdate)
import Game.Util (nowSeconds, maybeThrow)
import Graphics.CanvasAction (CanvasPattern, PatternRepeat(..), QuerySelector(..), clearRectFull, createPattern, fillRect, fillRectFull, filled, imageSource, querySelectContext2D, runActionOffscreen)
import Graphics.CanvasAction.Run (CTX, runCtx)
import Run.Reader (askAt)
import Run.State (get, modify)
import Type.Row (type (+))


type Extra = CTX + ()

type Env = CanvasPattern

type State = Vector2 Number

game ∷ AffGame Extra Void Unit
game = mkAffGame
  { init: ado
      pattern ← runActionOffscreen (20.0 >< 20.0) do
        filled "#aaf" fillRectFull
        filled "#afa" $ fillRect (makeRect 0.0 10.0 10.0 10.0)
        filled "#faa" $ fillRect (makeRect 10.0 0.0 10.0 10.0)
        imageSource >>= (_ `createPattern` Repeat)
      in { env: pattern ∷ Env
         , initState: (0.0 >< 0.0) ∷ State
         }
  , updates:
    [ onStart do
        log "This should just print once when the game starts"
    , animationFrameUpdate do
        pattern ← askAt _env
        clearRectFull
        dimensions ← get
        filled pattern $ fillRect (Rect zero dimensions)
    , everyUpdate (Milliseconds 100.0) do
        modify ((add 3.0 >< add 1.0) <*> _)
    , animationFrameMatchInterval (pure (FPS 2.0)) do
        modify ((identity >< add 12.0) <*> _)
        vector ← get
        (Seconds now) ← nowSeconds
        log ("Now: " <> show now <> ", Pos: " <> show vector)
    ]
  }


main ∷ Effect Unit
main = do
  ctx ← querySelectContext2D (QuerySelector "canvas#game")
     >>= maybeThrow "no canvas"
  launchGame_ (mkReducer do runCtx ctx) game
