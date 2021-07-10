-- todo: document whether updates will run once right away or defer its first
-- computation
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
  , unAffGame
  , local
  , ParAffGame
  , unParAffGame
  , simpleAffGame

  , AffGameUpdateM
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
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Error.Class as MonadThrow
import Control.Monad.Except (ExceptT)
import Control.Monad.Except as MonadExcept
import Control.Monad.Fork.Class (never)
import Control.Monad.Reader (class MonadAsk, ReaderT(..), ask, runReaderT)
import Control.Monad.Reader as MR
import Control.Monad.Reader as MonadReader
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Parallel (class Parallel, parOneOfMap, parallel, sequential)
import Control.Plus (class Alt, class Plus, alt, empty)
import Data.Either (Either(..), either)
import Data.Functor.Compose (Compose(..))
import Data.Newtype (class Newtype, over2)
import Data.Time.Duration (Seconds(..), class Duration, Milliseconds(..), convertDuration)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, ParAff, launchAff, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Game (GameUpdate(..), Reducer, mkReducer, runReducer) as Exports
import Game (GameUpdate(..), Reducer, mkRunGame, runReducer)
import Game.Util (forever, fromLeft, iterateM, newRef, nowSeconds, runStateWithRef)
import Run (AFF, EFFECT, Run, expand, runBaseAff, runBaseAff')
import Run (liftAff) as Run
import Run.Except (EXCEPT, Except, runExcept, runExceptAt, throw)
import Run.Reader (Reader, askAt, runReaderAt)
import Run.State (STATE)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))


_dt ∷ Proxy "dt"
_dt = Proxy

_end ∷ Proxy "end"
_end = Proxy

_stateRef ∷ Proxy "stateRef"
_stateRef = Proxy

_env ∷ Proxy "env"
_env = Proxy

-- todo: maybe rename all the `ExecIn`, `ExecOut`, etc. to something more
-- understandable
-- | The execIn effects that are supported by the `onStart` update
type OnStartExecIn ∷
  Type → Type → Type → Type → Row (Type → Type) → Row (Type → Type)
type OnStartExecIn env state err a r
  = STATE state + EXCEPT err + EFFECT + AFF +
  ( env ∷ Reader env
  , end ∷ Except a
  | r )

-- | The execIn effects that are supported by most loop updates
type LoopExecIn ∷
  Type → Type → Type → Type → Row (Type → Type) → Row (Type → Type)
type LoopExecIn env state err a r
  = STATE state + EXCEPT err + EFFECT + AFF +
  ( env ∷ Reader env
  , dt ∷ Reader Seconds
  , end ∷ Except a
  | r )

-- todo: convert the 3 type synonyms below to be open rows, for easier
-- composition? That also allows for the `*ExecIn` types to use `Rec` directly
-- | The execOut effects for `TemplateAffGame`
type ExecOut ∷ Type → Type → Type → Type → Row (Type → Type)
type ExecOut env state err a
  = EFFECT + AFF + EXCEPT err +
  ( stateRef ∷ Reader (Ref state)
  , env ∷ Reader env
  , end ∷ Except a
  )

-- | The effects in an interpreted `TemplateAffGame`
type Interpreted ∷ Type → Type → Type → Row (Type → Type)
type Interpreted env state err
  = EFFECT + AFF + EXCEPT err +
  ( stateRef ∷ Reader (Ref state)
  , env ∷ Reader env
  )

-- | The `effect`, `aff` and `except` effects are required to be supported by
-- | every `AffGameUpdate`. Reducers for `AffGame` can interpret the extra
-- | effects in terms of them.
type Req ∷ Type → Row (Type → Type)
type Req err = (EFFECT + AFF + EXCEPT err + ())


-- | An `AffGame` is a transformer stack consisting of a reader for a `Reducer`,
-- | an except for errors, and an aff. Can be constructed from a
-- | `TemplateAffGame` using `mkAffGame`, or using `simpleAffGame`.
newtype AffGame extra err a = AffGame
  (ReaderT (Reducer extra (Req err)) (ExceptT err Aff) a)

-- | Can't have a `NewType` instance because of
-- | https://github.com/purescript/purescript/issues/4101. So there is provided
-- | a manual deconstructor
unAffGame ∷
  ∀ extra err a
  . AffGame extra err a
  → ReaderT (Reducer extra (Req err)) (ExceptT err Aff) a
unAffGame (AffGame game) = game

-- No newtype instance, so we need to define all instances manually.
-- at least we don't need to name the instances anymore, thanks Jordan :)

-- derive instance Newtype (AffGame extra err a) _
instance Functor (AffGame extra err) where
  map f (AffGame a) = AffGame (f <$> a)

instance Apply (AffGame extra err) where
  apply (AffGame f) (AffGame a) = AffGame (f <*> a)

instance Applicative (AffGame extra err) where
  pure x = AffGame (pure x)

instance Bind (AffGame extra err) where
  bind (AffGame a) f = AffGame $ a >>= (f >>> unAffGame)

instance Monad (AffGame extra err)

instance MonadEffect (AffGame extra err) where
  liftEffect = AffGame <<< liftEffect

instance MonadAff (AffGame extra err) where
  liftAff = AffGame <<< liftAff

instance MonadRec (AffGame extra err) where
  tailRecM f a = AffGame $ tailRecM (unAffGame <<< f) a

instance MonadThrow err (AffGame extra err) where
  throwError e = AffGame (throwError e)

instance MonadError err (AffGame extra err) where
  catchError (AffGame a) f = AffGame $ catchError a (f >>> unAffGame)

instance MonadAsk (Reducer extra (Req err)) (AffGame extra err) where
  ask = AffGame ask
  
-- | Can't have a `MonadReader` instance because of
-- | https://github.com/purescript/purescript/issues/4101. So there is provided
-- | a manual `local`.
-- derive newtype instance MonadReader (Reducer extra (Req err)) (AffGame extra err)
local ∷
  ∀ extra err a
  . (Reducer extra (Req err) → Reducer extra (Req err))
  → AffGame extra err a
  → AffGame extra err a
local f (AffGame a) = AffGame $ MR.local f a

instance Semigroup err ⇒ Alt (AffGame extra err) where
  alt (AffGame a) (AffGame b) = AffGame (alt a b)

instance Monoid err ⇒ Plus (AffGame extra err) where
  empty = AffGame empty

instance Parallel (ParAffGame extra err) (AffGame extra err) where
  sequential =
    ( coerce ∷ ∀ a
      . ParAffGame extra err a
      → ((Reducer extra (Req err)) → ParAff (Either err a))
    ) >>>
    ( compose sequential ∷ ∀ a
      . ((Reducer extra (Req err)) → ParAff (Either err a))
      → ((Reducer extra (Req err)) → Aff (Either err a))
    ) >>>
    ( coerce ∷ ∀ a
      . ((Reducer extra (Req err)) → Aff (Either err a))
      → AffGame extra err a
    )
  parallel =
    ( coerce ∷ ∀ a
      . AffGame extra err a
      → ((Reducer extra (Req err)) → Aff (Either err a))
    ) >>>
    ( compose parallel ∷ ∀ a
      . ((Reducer extra (Req err)) → Aff (Either err a))
      → ((Reducer extra (Req err)) → ParAff (Either err a))
    ) >>>
    ( coerce ∷ ∀ a
      . ((Reducer extra (Req err)) → ParAff (Either err a))
      → ParAffGame extra err a
    )

instance Semigroup a ⇒ Semigroup (AffGame extra err a) where
  append = lift2 append

instance Monoid a ⇒ Monoid (AffGame extra err a) where
  mempty = AffGame mempty

instance Lazy (AffGame extra err a) where
  defer f = AffGame <<< ReaderT $
    \reducer → (runReaderT <<< unAffGame) (f unit) reducer


-- | A variant of `AffGame` that composes effects in parallel
newtype ParAffGame extra err a = ParAffGame
  (ReaderT (Reducer extra (Req err)) (Compose ParAff (Either err)) a)

-- | Can't have a `NewType` instance because of
-- | https://github.com/purescript/purescript/issues/4101. So there is provided
-- | a manual deconstructor
unParAffGame ∷
  ∀ extra err a
  . ParAffGame extra err a
  → ReaderT (Reducer extra (Req err)) (Compose ParAff (Either err)) a
unParAffGame (ParAffGame game) = game

-- derive instance Newtype (ParAffGame extra err a) _

instance Functor (ParAffGame extra err) where
  map f (ParAffGame a) = ParAffGame (f <$> a)

instance Apply (ParAffGame extra err) where
  apply (ParAffGame f) (ParAffGame a) = ParAffGame (f <*> a)

instance Applicative (ParAffGame extra err) where
  pure x = ParAffGame (pure x)

instance Alt (ParAffGame extra err) where
  alt (ParAffGame a) (ParAffGame b) = ParAffGame (alt a b)

instance Plus (ParAffGame extra err) where
  empty = ParAffGame empty

instance Semigroup a ⇒ Semigroup (ParAffGame extra err a) where
  append = lift2 append

instance Monoid a ⇒ Monoid (ParAffGame extra err a) where
  mempty = ParAffGame mempty

instance Lazy (ParAffGame extra err a) where
  defer f = ParAffGame <<< ReaderT $
    \reducer → (runReaderT <<< unParAffGame) (f unit) reducer

-- | Construct a simple `AffGame` using a `Run` whose effect row includes the
-- | `AffGame`'s `extra` row
simpleAffGame ∷
  ∀ extra err a
  . Run (EFFECT + AFF + EXCEPT err + extra) a
  → AffGame extra err a
simpleAffGame simpleGame = do
  reducer ← ask
  simpleGame
    # runReducer reducer
    # runExcept
    # runBaseAff'
    # liftAff
    # (=<<) (either MonadThrow.throwError pure)

type AffGameUpdateM extra env state err a b =
  GameUpdate extra (Req err) (ExecOut env state err a) b

-- todo: maybe instead of having updates return Unit, have them return Void
type AffGameUpdate extra env state err a =
  AffGameUpdateM extra env state err a Unit

-- todo: env and initState should be in the top-level record
type TemplateAffGame extra env state err a =
  { init ∷ Run (Req err) { env ∷ env, initState ∷ state }
  , updates ∷ Array (AffGameUpdate extra env state err a)
  }

interpretedAffGameToAff ∷
  ∀ env state err a
  . env
  → Ref state
  → Run (Interpreted env state err) a
  → Aff (Either err a)
interpretedAffGameToAff env stateRef
  =   runReaderAt _env env
  >>> runReaderAt _stateRef stateRef
  >>> runExcept
  >>> runBaseAff'

interpretAffGame ∷
  ∀ env state err a
  . Run (ExecOut env state err a) Unit
  → Run (Interpreted env state err) a
interpretAffGame execOut = forever execOut
  # runExceptAt _end
  # map fromLeft

parallelizeAffGame ∷
  ∀ env state err a
  . Array (Run (Interpreted env state err) a)
  → Run (Interpreted env state err) a
parallelizeAffGame updates = do
  stateRef ← askAt _stateRef
  env ← askAt _env
  updates
    # map (interpretedAffGameToAff env stateRef)
    # parOneOfMap try
    # (=<<) (either MonadThrow.throwError pure)
    # Run.liftAff
    # (=<<) (either throw pure)

-- | Make an `AffGame` from a `TemplateAffGame`
mkAffGame ∷
  ∀ extra env state err a
  . TemplateAffGame extra env state err a
  → AffGame extra err a
mkAffGame { init, updates } = AffGame do
  { env, initState } ← init
    # runExcept
    # runBaseAff'
    # liftAff
    # (=<<) (either MonadThrow.throwError pure)
  reducer ← MonadReader.ask
  stateRef ← newRef initState
  -- todo: maybe separate this part out into its own `run(Template)AffGame`
  -- function. If so, interpretedAffGameToAff should probably also be exported
  mkRunGame interpretAffGame parallelizeAffGame reducer updates
    # interpretedAffGameToAff env stateRef
    # liftAff
    # (=<<) (either MonadThrow.throwError pure)


-- todo: Change up these
-- runGame and runGameAff could be just one function that uses `MonadAff`.
-- Maybe not if we want to include the `EXCEPT`-
-- But they should be changed anyway
-- | Run an `AffGame` in `Run`
runGame ∷
  ∀ extra err a r
  . Reducer extra (Req err)
  → AffGame extra err a
  → Run (AFF + EXCEPT err + r) a
runGame reducer (AffGame game) = game
  # flip MonadReader.runReaderT reducer
  # MonadExcept.runExceptT
  # Run.liftAff
  # (=<<) (either throw pure)


-- | Run an `AffGame` in `Aff`
runGameAff ∷
  ∀ extra err a
  . Reducer extra (Req err)
  → AffGame extra err a
  → Aff (Either err a)
runGameAff reducer game = runGame reducer game
  # runExcept
  # runBaseAff

-- | Launch an `AffGame` in `Effect`, returning the `Fiber`.
launchGame ∷
  ∀ extra err a
  . Reducer extra (Req err)
  → AffGame extra err a
  → Effect (Fiber (Either err a))
launchGame reducer game = launchAff do runGameAff reducer game

-- todo: this should require an `AffGame` with return value `Unit` and error value `Void`.
-- todo: there should be an easy way to handle the error and run in effect.
-- | Launch an `AffGame` in `Effect`. Discards the result value.
launchGame_ ∷
  ∀ extra err a
  . Reducer extra (Req err)
  → AffGame extra err a
  → Effect Unit
launchGame_ reducer game = void do launchGame reducer game

-- | An `AffGameUpdate` that runs once when the `AffGame` starts
onStart ∷
  ∀ extra env state err a
  . Run (OnStartExecIn env state err a extra) Unit
  → AffGameUpdate extra env state err a
onStart effect = GameUpdate \reducer → do
  stateRef ← askAt _stateRef
  (runReducer reducer effect ∷ Run (OnStartExecIn env state err a ()) Unit)
    # runStateWithRef stateRef
    # expand
  Run.liftAff never

-- todo: could probably rewrite this. might also change its API, it might not
-- need a dedicated initialization function (investigate)
-- | Create an `AffGameUpdate` that runs in loops, using the `Aff Unit` as a
-- | waiting action inbetween
loopUpdate ∷
  ∀ extra env state err a b
  . Aff Unit
  → Run (LoopExecIn env state err a extra) b
  → (b → Run (LoopExecIn env state err a extra) b)
  → AffGameUpdate extra env state err a
loopUpdate wait init loop = GameUpdate \reducer → do
  stateRef ← askAt _stateRef
  let
    init' = (runReducer reducer init ∷ Run (LoopExecIn env state err a ()) b)
      # runReaderAt _dt (Seconds 0.0)
      # runStateWithRef stateRef
      # expand
    step { time, passThrough } = do
      now ← liftEffect nowSeconds
      passThrough' ←
        (runReducer reducer $ loop passThrough
          ∷ Run (LoopExecIn env state err a ()) b
        )
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
  ∀ extra env state err a
  . Aff Unit
  → Run (LoopExecIn env state err a extra) Unit
  → AffGameUpdate extra env state err a
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
  ∀ extra env state err a d
  . Duration d
  ⇒ Aff Unit
  → Run (LoopExecIn env state err a extra) d
  → Run (LoopExecIn env state err a extra) Unit
  → AffGameUpdate extra env state err a
matchInterval wait duration loop = loopUpdate wait (askAt _dt) \accDt' → do
  d ← duration
  dt ← askAt _dt
  let
    newAccDt = case accDt' <> dt, convertDuration d of
      Seconds accDt, Seconds frameTime
        | accDt < frameTime → Left accDt
        | frameTime <= 0.0 → Right 0.0
        | accDt > frameTime * 3.0 → Right 0.0
        | otherwise → Right (accDt - frameTime)
  Seconds <$> case newAccDt of
    Left accDt → pure accDt
    Right accDt → loop $> accDt


-- | A newtype with a `Duration` instance, where the stored number describes an
-- | amount of frames per second. Running `fromDuration` will return the amount
-- | of milliseconds a frame will take at that framerate. `fromDuration (FPS
-- | 0.0)` is `Milliseconds 0.0`.
newtype FPS = FPS Number

derive instance Newtype FPS _
derive newtype instance Eq FPS
derive newtype instance Ord FPS

instance Show FPS where
  show (FPS n) = "(FPS " <> show n <> ")"

instance Duration FPS where
  fromDuration (FPS n) = Milliseconds if n == 0.0
    then 0.0
    else 1000.0 / n
  toDuration (Milliseconds n) = FPS if n == 0.0
    then 0.0
    else 1000.0 / n
