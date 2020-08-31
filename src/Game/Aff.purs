module Game.Aff
  ( _dt
  , _end
  , _stateRef
  , _env
  , OnStartExecIn
  , LoopExecIn
  , ExecOut
  , Interpreted
  , Req

  , AffGame(..)
  , ParAffGame

  , simpleAffGame
  , AffGameUpdate
  , TemplateAffGame
  , interpretAffGame
  , parallelizeAffGame
  , mkAffGame
  , runGame
  , runGameAff
  , launchGame
  , launchGame_

  , onStart
  , loopUpdate
  , loopUpdate'
  , matchInterval

  , FPS(..)

  , module Exports
  ) where

import Prelude hiding (join)

import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError)
import Control.Monad.Fork.Class (class MonadBracket, class MonadFork, class MonadKill, bracket, fork, join, kill, never, suspend, uninterruptible)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Parallel (parOneOfMap, class Parallel, parallel, sequential)
import Control.Plus (class Alt, class Plus, empty, (<|>))
import Data.Either (either, Either(..))
import Data.Newtype (class Newtype, over, over2)
import Data.Time.Duration (Seconds(..), class Duration, Milliseconds(..), convertDuration)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, ParAff, launchAff, throwError, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Game (GameUpdate(..), Reducer, mkReducer, runReducer) as Exports
import Game (GameUpdate(..), Reducer, mkRunGame, runReducer)
import Game.Util (forever, fromLeft, iterateM, newRef, nowSeconds, runStateWithRef)
import Run (AFF, EFFECT, Run, SProxy(..), expand, runBaseAff')
import Run (liftAff) as Run
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
  ( state   ∷ STATE s
  , env     ∷ READER e
  , end     ∷ EXCEPT a
  , effect  ∷ EFFECT
  , aff     ∷ AFF
  | r )

-- | The execIn effects that are used for most loop updates
type LoopExecIn e s a r =
  ( state   ∷ STATE s
  , env     ∷ READER e
  , end     ∷ EXCEPT a
  , dt      ∷ READER Seconds
  , effect  ∷ EFFECT
  , aff     ∷ AFF
  | r )

-- | The execOut effects for `TemplateAffGame`
type ExecOut e s a =
  ( stateRef ∷ READER (Ref s)
  , env      ∷ READER e
  , end      ∷ EXCEPT a
  , effect   ∷ EFFECT
  , aff      ∷ AFF
  )

-- | The effects in an interpreted `TemplateAffGame`
type Interpreted e s =
  ( stateRef ∷ READER (Ref s)
  , env      ∷ READER e
  , effect   ∷ EFFECT
  , aff      ∷ AFF
  )

-- | The `effect` and `aff` effects are required to be supported by
-- | every `AffGameUpdate`. Reducers for `AffGame` can interpret the extra
-- | effects in terms of them.
type Req = (effect ∷ EFFECT, aff ∷ AFF)


-- | An `AffGame` is an `Aff` that takes a `Reducer` as an argument. Can be
-- | constructed from a `TemplateAffGame` using `mkAffGame`.
newtype AffGame extra a = AffGame (Reducer extra Req → Aff a)

derive instance newtypeAffGame ∷ Newtype (AffGame extra a) _

derive instance functorAffGame ∷ Functor (AffGame extra)

instance applyAffGame ∷ Apply (AffGame extra) where
  apply (AffGame f) (AffGame a) = AffGame \r → (f r <*> a r)

instance applicativeAffGame ∷ Applicative (AffGame extra) where
  pure x = liftAff (pure x)

instance bindAffGame ∷ Bind (AffGame extra) where
  bind (AffGame a) f = AffGame \r → (a r >>= f >>> runGameAff r)

instance monadAffGame ∷ Monad (AffGame extra)

instance monadEffectAffGame ∷ MonadEffect (AffGame extra) where
  liftEffect a = liftAff (liftEffect a)

instance monadAffAffGame ∷ MonadAff (AffGame extra) where
  liftAff a = AffGame \_ → a

instance monadRecAffGame ∷ MonadRec (AffGame extra) where
  tailRecM f a = AffGame \r → tailRecM (\a' → runGameAff r (f a')) a

instance monadThrowAffGame ∷ MonadThrow Error (AffGame extra) where
  throwError e = liftAff (throwError e)

instance monadErrorAffGame ∷ MonadError Error (AffGame extra) where
  catchError (AffGame a) f = AffGame \r → catchError (a r) (f >>> runGameAff r)

instance monadForkAffGame ∷ MonadFork Fiber (AffGame extra) where
  suspend (AffGame a) = AffGame \r → suspend (a r)
  fork    (AffGame a) = AffGame \r → fork    (a r)
  join    fiber       = liftAff (join fiber)

instance monadKillAffGame ∷ MonadKill Error Fiber (AffGame extra) where
  kill error fiber = liftAff (kill error fiber)

instance monadBracketAffGame ∷ MonadBracket Error Fiber (AffGame extra) where
  bracket (AffGame acquire) release run = AffGame \r →
    bracket (acquire r)
      (\c a → runGameAff r (release c a))
      (\a → runGameAff r (run a))
  uninterruptible k = AffGame \r ->
    uninterruptible (runGameAff r k)
  never = liftAff never

instance altAffGame ∷ Alt (AffGame extra) where
  alt (AffGame a) (AffGame b) = AffGame \r → a r <|> b r

instance plusAffGame ∷ Plus (AffGame extra) where
  empty = liftAff empty

instance semigroupAffGame ∷ Semigroup a ⇒ Semigroup (AffGame extra a) where
  append = lift2 append

instance monoidAffGame ∷ Monoid a ⇒ Monoid (AffGame extra a) where
  mempty = pure mempty

instance lazyAffGame ∷ Lazy (AffGame extra a) where
  defer f = pure unit >>= f

instance parallelAffGame ∷ Parallel (ParAffGame extra) (AffGame extra) where
  parallel = over AffGame (map parallel)
  sequential = over ParAffGame (map sequential)

-- | A variant of `AffGame` that composes effects in parallel
newtype ParAffGame extra a = ParAffGame (Reducer extra Req → ParAff a)

derive instance newtypeParAffGame ∷ Newtype (ParAffGame extra a) _

derive instance functorParAffGame ∷ Functor (ParAffGame extra)

instance applyParAffGame ∷ Apply (ParAffGame extra) where
  apply (ParAffGame a) (ParAffGame b) = ParAffGame \r → a r <*> b r

instance applicativeParAffGame ∷ Applicative (ParAffGame extra) where
  pure x = ParAffGame \_ → pure x

instance altParAffGame ∷ Alt (ParAffGame extra) where
  alt (ParAffGame a) (ParAffGame b) = ParAffGame \r → a r <|> b r

instance plusParAffGame ∷ Plus (ParAffGame extra) where
  empty = ParAffGame \_ → empty

instance semigroupParAffGame ∷ Semigroup a ⇒ Semigroup (ParAffGame extra a)
  where
    append = lift2 append

instance monoidParAffGame ∷ Monoid a ⇒ Monoid (ParAffGame extra a) where
  mempty = pure mempty


-- | Construct a simple `AffGame` using a `Run` whose effect row includes the
-- | `AffGame`'s `extra` row
simpleAffGame ∷
  ∀ extra a. Run (effect ∷ EFFECT, aff ∷ AFF | extra) a → AffGame extra a
simpleAffGame game = AffGame \reducer → game
  # runReducer reducer
  # runBaseAff'

type AffGameUpdate extra e s a =
  GameUpdate extra Req (ExecOut e s a) Unit

type TemplateAffGame extra e s a =
  { init    ∷ Run (effect ∷ EFFECT, aff ∷ AFF) { env ∷ e, initState ∷ s }
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
    # Run.liftAff

-- | Make an `AffGame` from a `TemplateAffGame`
mkAffGame ∷
  ∀ extra e s a
  . TemplateAffGame extra e s a
  → AffGame extra a
mkAffGame { init, updates } = AffGame \reducer → runBaseAff' do
  { env, initState } ← init
  stateRef ← newRef initState
  mkRunGame interpretAffGame parallelizeAffGame reducer updates
    # runReaderAt _stateRef stateRef
    # runReaderAt _env env

-- | Run an `AffGame` in `Run`
runGame ∷
  ∀ extra a r
  . Reducer extra Req
  → AffGame extra a
  → Run (aff ∷ AFF | r) a
runGame reducer game = Run.liftAff (runGameAff reducer game)

-- | Run an `AffGame` in `Aff`
runGameAff ∷
  ∀ extra a
  . Reducer extra Req
  → AffGame extra a
  → Aff a
runGameAff reducer (AffGame game) = game reducer

-- | Launch an `AffGame` in `Effect`, returning the `Fiber`.
launchGame ∷
  ∀ extra a
  . Reducer extra Req
  → AffGame extra a
  → Effect (Fiber a)
launchGame reducer game = launchAff do runGameAff reducer game

-- | Launch an `AffGame` in `Effect`. Discards the result value.
launchGame_ ∷
  ∀ extra a
  . Reducer extra Req
  → AffGame extra a
  → Effect Unit
launchGame_ reducer game = void do launchGame reducer game

-- | An `AffGameUpdate` that runs once when the `AffGame` starts
onStart ∷
  ∀ extra e s a
  . Run (OnStartExecIn e s a extra) Unit
  → AffGameUpdate extra e s a
onStart effect = GameUpdate \reducer → do
  stateRef ← askAt _stateRef
  (runReducer reducer effect ∷ Run (OnStartExecIn e s a ()) Unit)
    # runStateWithRef stateRef
    # expand
  Run.liftAff never

-- | Create an `AffGameUpdate` that runs in loops, using the `Aff Unit` as a
-- | waiting action inbetween
loopUpdate ∷
  ∀ extra e s a b
  . Aff Unit
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
    (\prev → step prev <* liftAff wait)
    ({ time: _, passThrough: _} <$> nowSeconds <*> init')

-- | Create a simple loop update that runs the same action repeatedly, using
-- | the `Aff Unit` as a waiting action inbetween
loopUpdate' ∷
  ∀ extra e s a
  . Aff Unit
  → Run (LoopExecIn e s a extra) Unit
  → AffGameUpdate extra e s a
loopUpdate' wait loop = loopUpdate wait (pure unit) (const loop)

-- | Create a loop that on every iteration (distinguished by the `Aff Unit`)
-- | tries to match the interval returned by the first `Run` action. It will not
-- | run the second `Run` action (the "update") when looping if the given
-- | duration has not passed since the last update was run.
-- |
-- | It compensates for
-- | lost time, so that if the duration is a `pure (Milliseconds 10.0)`, and
-- | the waiting action alternates between resolving after 12ms and 8ms, when
-- | the update is first run after 12ms, `matchInterval` will remember that it
-- | is 2ms behind, and run the update again when the waiting action resolves
-- | after another 8ms. Also, if the waiting action is long enough that 3 times
-- | the returned duration has passed since the last update, it will clear the
-- | accumulated time difference and run the update once. This is so that if
-- | the waiting action takes a lot longer to resolve one time, a bunch of
-- | updates won't pile up and run in quick succession (otherwise, this can
-- | happen if you switch away from a tab when using `delayFrame` as your
-- | waiting action, as it will stop updating until the tab becomes active
-- | again).
matchInterval ∷
  ∀ extra e s a d
  . Duration d
  ⇒ Aff Unit
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


-- | A newtype with a `Duration` instance, where the stored number describes an
-- | amount of frames per second. Running `fromDuration` will return the amount
-- | of milliseconds a frame will take at that framerate. `fromDuration (FPS
-- | 0.0)` is `Milliseconds 0.0`.
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