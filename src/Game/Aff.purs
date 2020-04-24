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
import Game.Util (newRef, nowSeconds, readRef, iterateM, writeRef, forever, fromLeft)
import Run (AFF, EFFECT, Run, SProxy(..), expand, liftAff, liftEffect, runBaseAff')
import Run.Except (EXCEPT, runExceptAt)
import Run.Reader (READER, runReaderAt, askAt)
import Run.State (STATE, runState, evalState)


_dt :: SProxy "dt"
_dt = SProxy

_end :: SProxy "end"
_end = SProxy

_stateRef :: SProxy "stateRef"
_stateRef = SProxy

type LoopExecIn s a r =
  ( state  :: STATE s
  , end    :: EXCEPT a
  , dt     :: READER Seconds
  , effect :: EFFECT
  | r )

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

-- | Run an `AffGame` in `Effect`. Discards the result value, since it can't be
-- | read synchronously.
runGameEffect
  :: forall extra s a
   . s
  -> Reducer extra Req
  -> AffGame extra s a
  -> Effect Unit
runGameEffect init reducer game = launchAff_ do runGameAff init reducer game


loopUpdate
  :: forall extra s a b
   . Run (effect :: EFFECT, aff :: AFF) Unit
  -> Run (LoopExecIn s a extra) b
  -> (b -> Run (LoopExecIn s a extra) b)
  -> AffGameUpdate extra s a
loopUpdate wait = mkUpdateS \initIn updateIn -> do
    stateRef <- askAt _stateRef
    let
      step :: (Tuple Seconds b) -> Run (ExecOut s a) (Tuple Seconds b)
      step (Tuple prevTime passThrough) = do
        now <- liftEffect nowSeconds
        state <- readRef stateRef
        (Tuple newState newPT) <- updateIn passThrough
          # runReaderAt _dt (now `over2 Seconds (-)` prevTime)
          # runState state
          # expand
        writeRef newState stateRef
        pure (Tuple now newPT)
      initIn' :: Run (ExecOut s a) b
      initIn' = do
        state <- readRef stateRef
        initIn
          # runReaderAt _dt (Seconds 0.0)
          # evalState state
          # expand
    iterateM
      (\prev -> step prev <* expand wait)
      (Tuple <$> nowSeconds <*> initIn')
  where
    mkUpdateS
      :: (        Run (LoopExecIn s a ()) b
         -> (b -> Run (LoopExecIn s a ()) b)
         -> Run (ExecOut s a) Unit )
      -> Run (LoopExecIn s a extra) b
      -> (b -> Run (LoopExecIn s a extra) b)
      -> GameUpdate extra Req (ExecOut s a)
    mkUpdateS = mkUpdate

loopUpdate'
  :: forall extra s a
   . Run (effect :: EFFECT, aff :: AFF) Unit
  -> Run (LoopExecIn s a extra) Unit
  -> AffGameUpdate extra s a
loopUpdate' wait update = loopUpdate wait (pure unit) (const update)

matchInterval
  :: forall extra s a d
   . Duration d
  => Run (effect :: EFFECT, aff :: AFF) Unit
  -> d
  -> Run (LoopExecIn s a extra) Unit
  -> AffGameUpdate extra s a
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