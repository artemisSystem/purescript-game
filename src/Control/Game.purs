module Control.Game where

import Prelude

import Control.Game.Util (newRef)
import Control.Parallel (parOneOfMap)
import Data.Either (either)
import Data.Maybe (Maybe)
import Data.List (List, (:))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError, try)
import Effect.Ref (Ref)
import Data.Newtype (class Newtype, over)
import Record (modify)


newtype Game s a = Game
  { init :: Aff s
  , update :: List (Ref s -> Aff a)
  }

derive instance newtypeGame :: Newtype (Game s a) _

runSimpleGame :: forall s a. Game s a -> Aff a
runSimpleGame (Game { init, update }) = do
  state <- init >>= newRef
  update # parOneOfMap do (_ $ state) >>> try
         # (=<<) (either throwError pure)

class ToGame s a g | g -> s a where
  toGame :: g -> Aff (Game s a)

instance toGameGame :: ToGame s a (Game s a) where
  toGame = pure

runGame :: forall game s a. ToGame s a game => game -> Aff a
runGame = toGame >=> runSimpleGame


class ToUpdate s a u | u -> s a where
  toUpdate :: u -> (Ref s -> Aff a)

addToGame :: forall s a u. ToUpdate s a u => u -> Game s a -> Game s a
addToGame u = over Game $ modify (SProxy :: _ "update") (toUpdate u : _)

-- TODO: unsure about this
type EffectGameUpdate s a =
  { step    :: s -> Effect s
  , render  :: s -> Effect Unit
  , resolve :: s -> Effect (Maybe a)
  }

-- TODO: should this go in this module? or somewhere else
-- after :: Duration d => d -> Effect a -> Aff a
-- every :: Duration d => d -> Effect Unit -> Aff Void
-- everyUntil :: Duration d => d -> Effect (Maybe a) -> Aff a