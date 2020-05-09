module Game.Aff where

import Prelude

import Control.Parallel (parOneOfMap)
import Data.Either (either, Either(..))
import Data.Newtype (class Newtype, over2)
import Data.Time.Duration (Seconds(..), class Duration, Milliseconds(..), convertDuration)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError, try, launchAff_)
import Effect.Ref (Ref)
import Game (Game, GameUpdate, Reducer, mkRunGame, mkUpdate)
import Game.Util (forever, fromLeft, iterateM, newRef, nowSeconds, runStateWithRef)
import Run (AFF, EFFECT, Run, SProxy(..), expand, liftAff, liftEffect, runBaseAff')
import Run.Except (EXCEPT, runExceptAt)
import Run.Reader (READER, runReaderAt, askAt)
import Run.State (STATE)


_dt :: SProxy "dt"
_dt = SProxy

_end :: SProxy "end"
_end = SProxy

_stateRef :: SProxy "stateRef"
_stateRef = SProxy

_env :: SProxy "env"
_env = SProxy

type LoopExecIn e s a r =
  ( state  :: STATE s
  , env    :: READER e
  , end    :: EXCEPT a
  , dt     :: READER Seconds
  , effect :: EFFECT
  | r )

type ExecOut e s a =
  ( stateRef :: READER (Ref s)
  , env      :: READER e
  , end      :: EXCEPT a
  , effect   :: EFFECT
  , aff      :: AFF
  )

type Interpreted e s =
  ( stateRef :: READER (Ref s)
  , env      :: READER e
  , effect   :: EFFECT
  , aff      :: AFF
  )

-- TODO: Have Req include state, env and end as well?
type Req = (effect :: EFFECT)

type AffGameUpdate extra e s a = GameUpdate extra Req (ExecOut e s a)

-- TODO: make init able to do more (end)
type AffGame extra e s a =
  { init    :: Run (effect :: EFFECT, aff :: AFF) (Tuple e s)
  , updates :: Game extra Req (ExecOut e s a)
  }


interpretAffGame
  :: forall e s a. Run (ExecOut e s a) Unit -> Run (Interpreted e s) a
interpretAffGame execOut = forever execOut
  # runExceptAt _end
  # map fromLeft

parallelizeAffGame
  :: forall e s a. Array (Run (Interpreted e s) a) -> Run (Interpreted e s) a
parallelizeAffGame games = do
  stateRef <- askAt _stateRef
  env <- askAt _env
  games
    # map (   runReaderAt _stateRef stateRef
          >>> runReaderAt _env env
          >>> runBaseAff' )
    # parOneOfMap try
    # (=<<) (either throwError pure)
    # liftAff

-- | Run an `AffGame` in `Run`
runGame
  :: forall extra r e s a
   . Reducer extra Req
  -> AffGame extra e s a
  -> Run (effect :: EFFECT, aff :: AFF | r) a
runGame reducer { init, updates } = expand do
  (Tuple env initState) <- init
  stateRef <- newRef initState
  mkRunGame interpretAffGame parallelizeAffGame reducer updates
    # runReaderAt _stateRef stateRef
    # runReaderAt _env env

-- | Run an `AffGame` in `Aff`
runGameAff
  :: forall extra e s a
   . Reducer extra Req
  -> AffGame extra e s a
  -> Aff a
runGameAff reducer game = runBaseAff' do runGame reducer game

-- | Run an `AffGame` in `Effect`. Discards the result value, since it can't be
-- | read synchronously.
runGameEffect
  :: forall extra e s a
   . Reducer extra Req
  -> AffGame extra e s a
  -> Effect Unit
runGameEffect reducer game = launchAff_ do runGameAff reducer game


loopUpdate
  :: forall extra e s a b
   . Run (effect :: EFFECT, aff :: AFF) Unit
  -> Run (LoopExecIn e s a extra) b
  -> (b -> Run (LoopExecIn e s a extra) b)
  -> AffGameUpdate extra e s a
loopUpdate wait = mkUpdateS \initIn updateIn -> do
    stateRef <- askAt _stateRef
    let
      step :: (Tuple Seconds b) -> Run (ExecOut e s a) (Tuple Seconds b)
      step (Tuple prevTime passThrough) = do
        now <- liftEffect nowSeconds
        newPT <- updateIn passThrough
          # runReaderAt _dt (now `over2 Seconds (-)` prevTime)
          # runStateWithRef stateRef
          # expand
        pure (Tuple now newPT)
      initIn' :: Run (ExecOut e s a) b
      initIn' = do
        initIn
          # runReaderAt _dt (Seconds 0.0)
          # runStateWithRef stateRef
          # expand
    iterateM
      (\prev -> step prev <* expand wait)
      (Tuple <$> nowSeconds <*> initIn')
  where
    mkUpdateS
      :: (        Run (LoopExecIn e s a ()) b
         -> (b -> Run (LoopExecIn e s a ()) b)
         -> Run (ExecOut e s a) Unit )
      -> Run (LoopExecIn e s a extra) b
      -> (b -> Run (LoopExecIn e s a extra) b)
      -> GameUpdate extra Req (ExecOut e s a)
    mkUpdateS = mkUpdate

loopUpdate'
  :: forall extra e s a
   . Run (effect :: EFFECT, aff :: AFF) Unit
  -> Run (LoopExecIn e s a extra) Unit
  -> AffGameUpdate extra e s a
loopUpdate' wait update = loopUpdate wait (pure unit) (const update)

-- TODO: wrap `d` in `Run`
matchInterval
  :: forall extra e s a d
   . Duration d
  => Run (effect :: EFFECT, aff :: AFF) Unit
  -> d
  -> Run (LoopExecIn e s a extra) Unit
  -> AffGameUpdate extra e s a
matchInterval wait duration update = loopUpdate wait (askAt _dt) \accDt' -> do
  dt <- askAt _dt
  let newAccDt = case accDt' <> dt, convertDuration duration of
        Seconds accDt, Seconds frameTime
          | accDt < frameTime       -> Left accDt
          | frameTime <= 0.0        -> Right 0.0
          | accDt > frameTime * 3.0 -> Right 0.0
          | otherwise               -> Right (accDt - frameTime)
  Seconds <$> case newAccDt of
    Left  accDt -> pure accDt
    Right accDt -> update $> accDt


newtype FPS = FPS Number

derive instance newtypeFPS :: Newtype FPS _
derive newtype instance eqFPS :: Eq FPS
derive newtype instance ordFPS :: Ord FPS

instance showFPS :: Show FPS where
  show (FPS n) = "(FPS " <> show n <> ")"

instance durationFPS :: Duration FPS where
  fromDuration (FPS n) = Milliseconds if n == 0.0
    then 0.0
    else 1000.0 / n
  toDuration (Milliseconds n) = FPS if n == 0.0
    then 0.0
    else 1000.0 / n