module Game.Aff.Web where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Game.Aff (AffGameUpdate, LoopExecIn, loopUpdate)
import Prim.Row (class Union)
import Run (Run, EFFECT, AFF, liftAff)
import Unsafe.Coerce (unsafeCoerce)
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
  :: forall extra update s a
   . Union (LoopExecIn s a) extra update
  => Window -> Run update Unit -> AffGameUpdate extra s a
animationFrameUpdate' w update = loopUpdate (delayFrame' w) (coerce update)
  where
    coerce :: Run update Unit -> Run _ Unit
    coerce = unsafeCoerce


delayFrameAff :: Aff Unit
delayFrameAff = liftEffect W.window >>= delayFrameAff'

delayFrame :: forall r. Run (effect :: EFFECT, aff :: AFF | r) Unit
delayFrame = liftEffect W.window >>= delayFrame'

animationFrameUpdate
  :: forall extra update s a
   . Union (LoopExecIn s a) extra update
  => Run update Unit -> AffGameUpdate extra s a
animationFrameUpdate update = loopUpdate delayFrame (coerce update)
  where
    coerce :: Run update Unit -> Run _ Unit
    coerce = unsafeCoerce

 