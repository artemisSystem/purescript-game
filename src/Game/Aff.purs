module Game.Aff where

import Prelude

import Control.Parallel (parOneOfMap)
import Data.Either (either, Either(..))
import Data.Newtype (class Newtype, over2)
import Data.Time.Duration (Seconds(..), class Duration, Milliseconds(..), convertDuration)
import Effect (Effect)
import Effect.Aff (Aff, throwError, try, launchAff_, never)
import Effect.Ref (Ref)
import Game (GameUpdate(..), Reducer, mkRunGame, runReducer)
import Game.Util (forever, fromLeft, iterateM, newRef, nowSeconds, runStateWithRef)
import Run (AFF, EFFECT, Run, SProxy(..), expand, liftAff, liftEffect, runBaseAff')
import Run.Except (EXCEPT, runExceptAt)
import Run.Reader (READER, runReaderAt, askAt)
import Run.State (STATE)


_dt ∷ SProxy "dt"
_dt = SProxy

_end ∷ SProxy "end"
_end = SProxy

_stateRef ∷ SProxy "stateRef"
_stateRef = SProxy

_env ∷ SProxy "env"
_env = SProxy

-- | The execIn effects that are used for the `onStart` update
type OnStartExecIn e s a r =
  ( state  ∷ STATE s
  , env    ∷ READER e
  , end    ∷ EXCEPT a
  , effect ∷ EFFECT
  | r )

-- | The execIn effects that are used for most loop updates
type LoopExecIn e s a r =
  ( state  ∷ STATE s
  , env    ∷ READER e
  , end    ∷ EXCEPT a
  , dt     ∷ READER Seconds
  , effect ∷ EFFECT
  | r )

-- | The execOut effects for AffGame
type ExecOut e s a =
  ( stateRef ∷ READER (Ref s)
  , env      ∷ READER e
  , end      ∷ EXCEPT a
  , effect   ∷ EFFECT
  , aff      ∷ AFF
  )

-- | The effects in an interpreted AffGame
type Interpreted e s =
  ( stateRef ∷ READER (Ref s)
  , env      ∷ READER e
  , effect   ∷ EFFECT
  , aff      ∷ AFF
  )

-- | These effects are required to be supported by every AffGameUpdate. Reducers
-- | for AffGame can interpret the extra effects in terms of any of these
type Req e s a =
  ( effect ∷ EFFECT
  , state  ∷ STATE s
  , env    ∷ READER e
  , end    ∷ EXCEPT a
  )

type AffGameUpdate extra e s a =
  GameUpdate extra (Req e s a) (ExecOut e s a) Unit

type AffGame extra e s a =
  { init    ∷ Run (effect ∷ EFFECT, aff ∷ AFF) { env :: e, initState :: s }
  , updates ∷ Array (AffGameUpdate extra e s a)
  }


interpretAffGame ∷ ∀ e s a. Run (ExecOut e s a) Unit → Run (Interpreted e s) a
interpretAffGame execOut = forever execOut
  # runExceptAt _end
  # map fromLeft

parallelizeAffGame ∷
  ∀ e s a. Array (Run (Interpreted e s) a) → Run (Interpreted e s) a
parallelizeAffGame games = do
  stateRef ← askAt _stateRef
  env ← askAt _env
  games
    # map (   runReaderAt _stateRef stateRef
          >>> runReaderAt _env env
          >>> runBaseAff' )
    # parOneOfMap try
    # (=<<) (either throwError pure)
    # liftAff

-- | Run an `AffGame` in `Run`
runGame ∷
  ∀ extra r e s a
  . Reducer extra (Req e s a)
  → AffGame extra e s a
  → Run (effect ∷ EFFECT, aff ∷ AFF | r) a
runGame reducer { init, updates } = expand do
  { env, initState } ← init
  stateRef ← newRef initState
  mkRunGame interpretAffGame parallelizeAffGame reducer updates
    # runReaderAt _stateRef stateRef
    # runReaderAt _env env

-- | Run an `AffGame` in `Aff`
runGameAff ∷
  ∀ extra e s a
  . Reducer extra (Req e s a)
  → AffGame extra e s a
  → Aff a
runGameAff reducer game = runBaseAff' do runGame reducer game

-- | Run an `AffGame` in `Effect`. Discards the result value, since it can't be
-- | read synchronously.
runGameEffect ∷
  ∀ extra e s a
  . Reducer extra (Req e s a)
  → AffGame extra e s a
  → Effect Unit
runGameEffect reducer game = launchAff_ do runGameAff reducer game


onStart ∷
  ∀ extra e s a
  . Run (OnStartExecIn e s a extra) Unit
  → AffGameUpdate extra e s a
onStart effect = GameUpdate \reducer → do
  stateRef ← askAt _stateRef
  (runReducer reducer effect ∷ Run (OnStartExecIn e s a ()) Unit)
    # runStateWithRef stateRef
    # expand
  liftAff never

loopUpdate ∷
  ∀ extra e s a b
  . Run (effect ∷ EFFECT, aff ∷ AFF) Unit
  → Run (LoopExecIn e s a extra) b
  → (b → Run (LoopExecIn e s a extra) b)
  → AffGameUpdate extra e s a
loopUpdate wait init loop = GameUpdate \reducer → do
  stateRef ← askAt _stateRef
  let
    init' = (runReducer reducer init ∷ Run (LoopExecIn e s a ()) b)
      # runReaderAt _dt (Seconds 0.0)
      # runStateWithRef stateRef
      # expand
    step { time, passThrough } = do
      now ← liftEffect nowSeconds
      passThrough' ←
        (runReducer reducer $ loop passThrough ∷ Run (LoopExecIn e s a ()) b)
          # runReaderAt _dt (now `over2 Seconds (-)` time)
          # runStateWithRef stateRef
          # expand
      pure { time: now, passThrough: passThrough' }
  iterateM
    (\prev → step prev <* expand wait)
    ({ time: _, passThrough: _} <$> nowSeconds <*> init')

loopUpdate' ∷
  ∀ extra e s a
  . Run (effect ∷ EFFECT, aff ∷ AFF) Unit
  → Run (LoopExecIn e s a extra) Unit
  → AffGameUpdate extra e s a
loopUpdate' wait loop = loopUpdate wait (pure unit) (const loop)

matchInterval ∷
  ∀ extra e s a d
  . Duration d
  ⇒ Run (effect ∷ EFFECT, aff ∷ AFF) Unit
  → Run (LoopExecIn e s a extra) d
  → Run (LoopExecIn e s a extra) Unit
  → AffGameUpdate extra e s a
matchInterval wait duration loop = loopUpdate wait (askAt _dt) \accDt' → do
  d ← duration
  dt ← askAt _dt
  let newAccDt = case accDt' <> dt, convertDuration d of
        Seconds accDt, Seconds frameTime
          | accDt < frameTime       → Left accDt
          | frameTime <= 0.0        → Right 0.0
          | accDt > frameTime * 3.0 → Right 0.0
          | otherwise               → Right (accDt - frameTime)
  Seconds <$> case newAccDt of
    Left  accDt → pure accDt
    Right accDt → loop $> accDt


newtype FPS = FPS Number

derive instance newtypeFPS ∷ Newtype FPS _
derive newtype instance eqFPS ∷ Eq FPS
derive newtype instance ordFPS ∷ Ord FPS

instance showFPS ∷ Show FPS where
  show (FPS n) = "(FPS " <> show n <> ")"

instance durationFPS ∷ Duration FPS where
  fromDuration (FPS n) = Milliseconds if n == 0.0
    then 0.0
    else 1000.0 / n
  toDuration (Milliseconds n) = FPS if n == 0.0
    then 0.0
    else 1000.0 / n