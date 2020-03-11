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
import Game (Game, GameUpdate, mkRunGame)
import Game (Game, GameUpdate) as Exports
import Game.Util (newRef, nowSeconds, readRef, untilRight, writeRef)
import Run (AFF, EFFECT, Run, SProxy(..), expand, liftAff, liftEffect, runBaseAff')
import Run.Except (EXCEPT, runExceptAt)
import Run.Reader (READER, runReaderAt, askAt)
import Run.State (STATE, runState)

-- | The row of game effects used for `AffGame`. Most of the functions in this
-- | library that create `GameUpdate`s use this row. Each "looper" decides how
-- | these effects are used, but they are usually used like this:
-- |
-- | - `state` is a STATE effect that gives access to the game state
-- | - `dt` is a READER effect that gives the time since the last update in
-- | seconds
-- | - `end` is an EXCEPT effect. When an exception is thrown, the game ends
-- | - `effect` lets the update use arbitrary effects
type GameEffects s a =
  ( state  :: STATE s
  , dt     :: READER Seconds
  , end    :: EXCEPT a
  , effect :: EFFECT
  )

type Looper s = (stateRef :: READER (Ref s), effect :: EFFECT, aff :: AFF)

_stateRef :: SProxy "stateRef"
_stateRef = SProxy

-- should remain with four fields: STATE for updating state, EXCEPT for
-- terminating the game, READER for reading dt, and EFFECT for running effects.
-- this allows for elegant handling of for example canvas.
-- r can be a huge row when adding them, but then before running the game,
-- they need to be reduced to ge.
type AffGame ge s a = Game ge ge (Looper s) a

-- | Run an `AffGame` in `Run`
runGame
  :: forall ge s a
   . s -> AffGame ge s a -> Run (effect :: EFFECT, aff :: AFF) a
runGame init game = do
  stateRef <- newRef init
  game # mkRunGame
    do runReaderAt _stateRef stateRef
    do parOneOfMap (runBaseAff' >>> try)
         >>> (=<<) (either throwError pure)
         >>> liftAff

-- | Run an `AffGame` in `Aff`
runGameAff :: forall ge s a. s -> Game ge ge (Looper s) a -> Aff a
runGameAff init = runGame init >>> runBaseAff'

-- | Run an `AffGame` in `Effect`
runGameEffect :: forall ge s a. s -> Game ge ge (Looper s) a -> Effect Unit
runGameEffect init = runGameAff init >>> launchAff_

_end :: SProxy "end"
_end = SProxy

_dt :: SProxy "dt"
_dt = SProxy


loopAction
  :: forall s a
   . (Run (effect :: EFFECT) ~> Run (effect :: EFFECT, aff :: AFF))
  -> Run (GameEffects s a) Unit
  -> Run (Looper s) a
loopAction single update = do
  stateRef <- askAt _stateRef
  let
    step t0 = do
      t <- liftEffect nowSeconds
      state <- readRef stateRef
      result <- update
        # runReaderAt _dt (t `over2 Seconds (-)` t0)
        # runState state >>> map fst
        # runExceptAt _end
      case result of
        Left a -> pure (Right a)
        Right s -> writeRef s stateRef $> Left t
  untilRight
    do \t0 -> expand $ single (step t0)
    do liftEffect nowSeconds <#> Left

loopUpdate
  :: forall r s a
   . (Run (effect :: EFFECT) ~> Run (effect :: EFFECT, aff :: AFF))
  -> Run r Unit
  -> GameUpdate (GameEffects s a) r (Looper s) a
loopUpdate single update =
  { update
  , loop: loopAction single
  }