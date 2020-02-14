module Control.Game.Types where

import Prelude

import Control.Game.Util (newRef, readRef, writeRef)
import Control.Parallel (parOneOfMap)
import Data.Either (either)
import Data.Foldable (class Foldable, foldr)
import Data.Maybe (Maybe)
import Data.List (List, singleton, (:))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError, try)
import Effect.Ref (Ref)
import Data.Newtype (class Newtype, over)
import Record (modify)


newtype Game s a = Game
  { init   :: Aff s
  , update :: List (Ref s -> Aff a)
  }

derive instance newtypeGame :: Newtype (Game s a) _

runSimpleGame :: forall s a. Game s a -> Aff a
runSimpleGame (Game { init, update }) = do
  state <- init >>= newRef
  update # parOneOfMap do (_ $ state) >>> try
         # (=<<) (either throwError pure)

class ToGame s a g | g -> s where
  toGame :: g -> Aff (Game s a)

instance toGameGame :: ToGame s a (Game s a) where
  toGame = pure

instance toGameAff :: ToGame Unit a (Aff a) where
  toGame aff = pure $ Game
    { init: pure unit
    , update: singleton \_ -> aff
    }

runGame :: forall game s a. ToGame s a game => game -> Aff a
runGame = toGame >=> runSimpleGame


class ToUpdate s a u where
  toUpdate :: u -> (Ref s -> Aff a)

instance toUpdateRefAff :: ToUpdate s a (Ref s -> Aff a) where
  toUpdate = identity

instance toUpdateAff :: ToUpdate s a (Aff a) where
  toUpdate = const

addToGame :: forall s a u. ToUpdate s a u => u -> Game s a -> Game s a
addToGame u = over Game $ modify (SProxy :: _ "update") (toUpdate u : _)

infixr 5 addToGame as :+

addMultipleToGame
  :: forall f s a u
   . Foldable f => ToUpdate s a u
  => f u -> Game s a -> Game s a
addMultipleToGame us game = foldr (:+) game us

infixr 5 addMultipleToGame as :*


type EffectUpdate s a =
  { step    :: s -> Effect s
  , render  :: s -> Effect Unit
  , resolve :: s -> Effect (Maybe a)
  }

toEffect :: forall s a. EffectUpdate s a -> (Ref s -> Effect (Maybe a))
toEffect { step, render, resolve } = \ref -> do
  s <- readRef ref
  s' <- step s
  writeRef s' ref
  render s'
  resolve s'
