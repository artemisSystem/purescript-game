module Game.Aff.Web where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (class Duration)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Game.Aff (AffGameUpdate, LoopExecIn, loopUpdate', matchInterval)
import Run (Run, EFFECT, AFF, liftAff)
import Web.HTML (window) as W
import Web.HTML.Window (requestAnimationFrame, cancelAnimationFrame) as W
import Web.HTML.Window (Window)

delayFrameAff' :: Window -> Aff Unit
delayFrameAff' w = makeAff \cb -> do
  id <- W.requestAnimationFrame (cb (Right unit)) w
  pure $ effectCanceler do
    W.cancelAnimationFrame id w

delayFrame' :: forall r. Window -> Run (aff :: AFF | r) Unit
delayFrame' = delayFrameAff' >>> liftAff

animationFrameUpdate'
  :: forall extra s a
   . Window -> Run (LoopExecIn s a extra) Unit -> AffGameUpdate extra s a
animationFrameUpdate' w update = loopUpdate' (delayFrame' w) update

delayFrameAff :: Aff Unit
delayFrameAff = liftEffect W.window >>= delayFrameAff'

delayFrame :: forall r. Run (effect :: EFFECT, aff :: AFF | r) Unit
delayFrame = liftEffect W.window >>= delayFrame'

animationFrameUpdate
  :: forall extra s a
   . Run (LoopExecIn s a extra) Unit -> AffGameUpdate extra s a
animationFrameUpdate = loopUpdate' delayFrame

animationFrameMatchInterval
  :: forall extra s a d
   . Duration d
  => d -> Run (LoopExecIn s a extra) Unit -> AffGameUpdate extra s a
animationFrameMatchInterval = matchInterval delayFrame
