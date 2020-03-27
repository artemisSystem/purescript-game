module Game.Aff where

import Prelude

import Control.Parallel (parOneOfMap)
import Data.Either (Either(..), either)
import Data.Newtype (over2)
import Data.Time.Duration (Seconds(..))
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Aff, throwError, try, launchAff_)
import Effect.Ref (Ref)
import Game
import Game.Util (newRef, nowSeconds, readRef, untilRight, writeRef, forever, fromLeft)
import Run (AFF, EFFECT, Run, SProxy(..), expand, liftAff, liftEffect, runBaseAff')
import Run.Except (EXCEPT, runExceptAt)
import Run.Reader (READER, runReaderAt, askAt)
import Run.State (STATE, runState)
import Unsafe.Coerce

type Req = (effect :: EFFECT)

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

-- | `dt` isn't used anywhere in this module, but multiple `GameUpdate`s in
-- | other modules use it, so it's defined here so that each module doesn't need
-- | to define its own.
_dt :: SProxy "dt"
_dt = SProxy

_end :: SProxy "end"
_end = SProxy

_stateRef :: SProxy "stateRef"
_stateRef = SProxy


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

-- | Run a `Game` with this module's `Req` and `ExecOut` in `Run`
runGame
  :: forall extra s a
   . s
  -> Reducer extra Req
  -> Game extra Req (ExecOut s a)
  -> Run (effect :: EFFECT, aff :: AFF) a
runGame init reducer game = do
  stateRef <- newRef init
  mkRunGame interpretAffGame parallelizeAffGame reducer game
    # runReaderAt _stateRef stateRef

-- | Run a `Game` with this module's `Req` and `ExecOut` in `Aff`
runGameAff
  :: forall extra s a
   . s
  -> Reducer extra Req
  -> Game extra Req (ExecOut s a)
  -> Aff a
runGameAff state reducer game = runBaseAff' do runGame state reducer game

-- | Run a `Game` with this module's `Req` and `ExecOut` in `Effect`
runGameEffect
  :: forall extra s a
   . s
  -> Reducer extra Req
  -> Game extra Req (ExecOut s a)
  -> Effect Unit
runGameEffect state reducer game = launchAff_ do runGameAff state reducer game

