module Control.Game where

import Prelude

import Control.Plus
import Control.Game.Util (newRef, readRef, writeRef)
import Control.Parallel
import Control.Monad.Error.Class
import Control.Monad.Loops (untilJust)
import Control.Monad.Rec.Class (forever)
import Data.Either (either)
import Data.Foldable
import Data.Maybe (Maybe)
import Data.List (List(..))
import Effect
import Effect.Aff (Aff, Canceler, forkAff, supervise)
import Effect.Ref (Ref)

-- TODO: GameComponent class, with an addToGame method?


newtype GenericGame s a = GenericGame
  { init :: Aff s
  , update :: List (Ref s -> Aff a)
  }

runGenericGame :: forall s a. GenericGame s a -> Aff a
runGenericGame (GenericGame { init, update }) = do
  state <- init >>= newRef
  update
    # parOneOfMap do (_ $ state) >>> try
    # (=<<) (either throwError pure)


class Game s a game | game -> s, game -> a where
  toGenericGame :: game -> Aff (GenericGame s a)

instance gameGenericGame :: Game s a (GenericGame s a) where
  toGenericGame = pure

runGame :: forall game s a. Game s a game => game -> Aff a
runGame = toGenericGame >=> runGenericGame