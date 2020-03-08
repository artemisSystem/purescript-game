module Control.Game where
-- maybe rename this to Control.Game.Base since it looks like it will be very small

import Prelude

import Control.Game.Util (newRef)
import Control.Monad.Free (Free, foldFree, hoistFree)
import Control.Parallel (parOneOfMap)
import Data.Either (either)
import Data.Newtype (class Newtype, over, over2)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, throwError, try)
import Effect.Ref (Ref)
import Record (modify)
import Run (EFFECT, Run)
import Run.State (STATE)
import Run.Except (EXCEPT)


-- should remain with three fields: STATE, for updating state, EXCEPT, for
-- terminating the game, and EFFECT, for running effects.
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
newtype GameF ge r s a = GameF (Array (GameUpdate ge r s a))

derive instance newtypeGameF :: Newtype (GameF ge r s a) _

instance semigroupGameF :: Semigroup (GameF ge r s a) where
  append = over2 GameF append

instance monoidGameF :: Monoid (GameF ge r s a) where
  mempty = GameF []

type Game ge r s = Free (GameF ge r s)

-- | Run a game where the row of all the used effects are the same as the game
-- | updates row
runGame :: forall ge s a. s -> Game ge ge s a -> Aff a
runGame init = foldFree \(GameF updates) -> do
  state <- newRef init
  updates # parOneOfMap do \{ update, loop } -> try $ loop update state
          # (=<<) (either throwError pure)

-- | Change the effects that are used in a `Game`
mapUpdates
  :: forall r1 r2 ge s a
   . (Run r1 Unit -> Run r2 Unit) -> Game ge r1 s a -> Game ge r2 s a
mapUpdates f = hoistFree do over GameF (_ <#> modify (SProxy :: _ "update") f)

-- | The most common row of game effects. Most of the functions in this
-- | library that create `GameUpdate`s use this row.
type GameEffects s a = (state :: STATE s, end :: EXCEPT a, effect :: EFFECT)

_end :: SProxy "end"
_end = SProxy

-- | A GameUpdate is a state update to happen every frame, and an aff to run it
type GameUpdate ge r s a =
  { update :: Run r Unit
  , loop   :: Run ge Unit -> Ref s -> Aff a
  }