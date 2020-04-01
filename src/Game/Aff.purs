module Game.Aff where

import Prelude

import Control.Parallel (parOneOfMap)
import Data.Either (either)
import Data.Newtype (over2)
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError, try, launchAff_)
import Effect.Ref (Ref)
import Game (Game, GameUpdate, Reducer, mkRunGame, mkUpdate)
import Game.Util (newRef, nowSeconds, readRef, iterateM, writeRef, forever, fromLeft)
import Prim.Row (class Union)
import Run (AFF, EFFECT, Run, SProxy(..), expand, liftAff, liftEffect, runBaseAff')
import Run.Except (EXCEPT, runExceptAt)
import Run.Reader (READER, runReaderAt, askAt)
import Run.State (STATE, execState)


_dt :: SProxy "dt"
_dt = SProxy

_end :: SProxy "end"
_end = SProxy

_stateRef :: SProxy "stateRef"
_stateRef = SProxy

type LoopExecIn s a =
  ( state  :: STATE s
  , end    :: EXCEPT a
  , dt     :: READER Seconds
  , effect :: EFFECT
  )

type ExecOut s a =
  ( stateRef :: READER (Ref s)
  , end      :: EXCEPT a
  , effect   :: EFFECT
  , aff      :: AFF
  )

type Interpreted s =
  ( stateRef :: READER (Ref s)
  , effect   :: EFFECT
  , aff      :: AFF
  )

type Req = (effect :: EFFECT)

type AffGameUpdate extra s a = GameUpdate extra Req (ExecOut s a)

type AffGame extra s a = Game extra Req (ExecOut s a)


interpretAffGame :: forall s a. Run (ExecOut s a) Unit -> Run (Interpreted s) a
interpretAffGame execOut = forever execOut
  # runExceptAt _end
  # map fromLeft

parallelizeAffGame
  :: forall s a. Array (Run (Interpreted s) a) -> Run (Interpreted s) a
parallelizeAffGame games = do
  stateRef <- askAt _stateRef
  games
    # map (runReaderAt _stateRef stateRef >>> runBaseAff')
    # parOneOfMap try
    # (=<<) (either throwError pure)
    # liftAff

-- | Run an `AffGame` in `Run`
runGame
  :: forall extra s a
   . s
  -> Reducer extra Req
  -> AffGame extra s a
  -> Run (effect :: EFFECT, aff :: AFF) a
runGame init reducer game = do
  stateRef <- newRef init
  mkRunGame interpretAffGame parallelizeAffGame reducer game
    # runReaderAt _stateRef stateRef

-- | Run an `AffGame` in `Aff`
runGameAff
  :: forall extra s a
   . s
  -> Reducer extra Req
  -> AffGame extra s a
  -> Aff a
runGameAff init reducer game = runBaseAff' do runGame init reducer game

-- | Run an `AffGame` in `Effect`
runGameEffect
  :: forall extra s a
   . s
  -> Reducer extra Req
  -> AffGame extra s a
  -> Effect Unit
runGameEffect init reducer game = launchAff_ do runGameAff init reducer game

loopUpdate
  :: forall extra update s a
   . Union (LoopExecIn s a) extra update
  => Run (effect :: EFFECT, aff :: AFF) Unit
  -> Run update Unit
  -> AffGameUpdate extra s a
loopUpdate wait = mkUpdate \execIn -> do
  stateRef <- askAt _stateRef
  let
    step :: Seconds -> Run (ExecOut s a) Seconds
    step prev = do
      now <- liftEffect nowSeconds
      state <- readRef stateRef
      newState <- execIn
        # runReaderAt _dt (now `over2 Seconds (-)` prev)
        # execState state
        # expand
      writeRef newState stateRef
      pure now
  iterateM (\prev -> step prev <* expand wait) nowSeconds


-- TODO: `FPS` type, with `Duration` instance
