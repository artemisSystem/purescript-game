module Control.Game where

import Prelude

import Control.Game.Util (newRef, nowSeconds, readRef, untilRight, writeRef)
import Control.Monad.Free (Free, foldFree, hoistFree)
import Control.Parallel (parOneOfMap)
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, over, over2, un)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds(..))
import Data.Tuple (fst)
import Effect.Aff (Aff, throwError, try)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Prim.Row (class Union)
import Record (modify)
import Run
import Run.Except (EXCEPT, runExceptAt)
import Run.Reader (READER, runReaderAt)
import Run.State (STATE, runState)

-- should remain with four fields: STATE for updating state, EXCEPT for
-- terminating the game, READER for reading dt, and EFFECT for running effects.
-- this allows for elegant handling of for example canvas.
-- r can be a huge row when adding them, but then before running the game,
-- they need to be reduced to ge.

-- Game:
-- monoid to build up multiple state updates that run concurrently
-- monad to sequence games after eachother

-- ge is all the effects the game knows how to handle
  -- common functions exist for working with games that have state, except, and effect
-- r is all the effects in the game
-- s is the type of the game state
-- a is the game return type
newtype GameF ge r l a = GameF (Array (GameUpdate ge r l a))

derive instance newtypeGameF :: Newtype (GameF ge r s a) _

instance semigroupGameF :: Semigroup (GameF ge r s a) where
  append = over2 GameF append

instance monoidGameF :: Monoid (GameF ge r s a) where
  mempty = GameF []

type Game ge r s = Free (GameF ge r s)

mkRunGame
  :: forall ge l ig a
   . (Run l ~> Run ig)
  -> (forall a. Array (Run ig a) -> Run ig a)
  -> Game ge ge l a
  -> Run ig a
mkRunGame runLooper parallelize =
  foldFree do un GameF >>> map (runUpdate >>> runLooper) >>> parallelize

-- | Run a game where the row of all the used effects are the same as the game
-- | updates row
runGame :: forall ge s a. s -> Game ge ge (Looper s) a -> Run InterpretedGame a
runGame init game = do
  stateRef <- newRef init
  game
    # foldFree (\(GameF updates) -> updates
      # parOneOfMap (\{ update, loop } -> try $ (runReaderAt _stateRef stateRef >>> runBaseAff') (loop update))
      # (=<<) (either throwError pure))
    # liftAff
-- TODO:
-- change runGame to :: Game ge ge s a -> Run (effect :: EFFECT, aff :: AFF) a
-- runGameAff :: Game ge ge s a -> Aff a
-- runGameEffect :: Game ge ge s a -> Effect Unit

-- | Change the effects that are used in a `Game`
mapUpdates
  :: forall r1 r2 ge s a
   . (Run r1 Unit -> Run r2 Unit) -> Game ge r1 s a -> Game ge r2 s a
mapUpdates f = hoistFree do over GameF (_ <#> modify (SProxy :: _ "update") f)

-- | The most common row of game effects. Most of the functions in this
-- | library that create `GameUpdate`s use this row. Each "update runner"
-- | decides how these effects are used, but they are usually used like this:
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

type InterpretedGame = (effect :: EFFECT, aff :: AFF)

type Looper s = (stateRef :: READER (Ref s), effect :: EFFECT, aff :: AFF)

_stateRef :: SProxy "stateRef"
_stateRef = SProxy


_end :: SProxy "end"
_end = SProxy

_dt :: SProxy "dt"
_dt = SProxy

-- TODO: change desc
-- | A GameUpdate consists of a state update, and an `Aff` that controls when
-- | it's run
type GameUpdate ge r l a =
  { update :: Run r Unit
  , loop   :: Run ge Unit -> Run l a
  }

runUpdate :: forall ge l a. GameUpdate ge ge l a -> Run l a
runUpdate { update, loop } = loop update

reduceUpdate
  :: forall ge1 ge2 gex r s a
   . Union ge1 gex ge2
  => GameUpdate ge2 r s a -> GameUpdate ge1 r s a
reduceUpdate = modify (SProxy :: _ "loop") (expand >>> _)

loopAction
  :: forall s a
   . (Run (effect :: EFFECT) ~> Aff)
  -> Run (GameEffects s a) Unit
  -> Ref s
  -> Aff a
loopAction single update stateRef = do
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
    do \t0 -> single (step t0)
    do liftEffect nowSeconds <#> Left

loopUpdate
  :: forall r s a
   . (Run (effect :: EFFECT) ~> Aff)
  -> Run r Unit
  -> GameUpdate (GameEffects s a) r s a
loopUpdate single update =
  { update
  , loop: loopAction single
  }