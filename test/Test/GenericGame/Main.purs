module Test.GenericGame.Main where

import Prelude

import Control.Game (GenericGame(..), runGame)
import Control.Game.Util (modifyRef)
import Control.Monad.Loops (untilJust)
import Data.List (singleton)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = launchAff_ <<< runGame $ GenericGame
  { init: pure 0
  , update: singleton \s -> untilJust do
      delay (Milliseconds 100.0)
      n <- modifyRef (_ + 1) s
      logShow n
      pure if n >= 10 then Just unit else Nothing
  }