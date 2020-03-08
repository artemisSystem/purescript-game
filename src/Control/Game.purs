module Control.Game where

import Prelude

import Control.Game.Util (newRef, readRef, writeRef)
import Control.Parallel (parOneOfMap)
import Data.Either (either)
import Data.Foldable (class Foldable, foldr)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError, try)
import Effect.Ref (Ref)
import Data.Newtype (class Newtype, over)
import Record (modify)
import Run (VariantF, Run, runBaseAff, AFF, FProxy)


newtype Game  = Game 