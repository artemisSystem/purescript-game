module Game.Aff.Run
  ( AFFGAME
  , _affGame
  , liftAffGame
  , liftAffGameAt
  , runAffGame
  , runAffGameAt
  , runBaseAffGame
  , runBaseAffGameAt
  , runBaseAffGame'
  , runBaseAffGameAt'
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect.Aff.Class as Aff
import Effect.Class as Effect
import Game.Aff (AffGame, AffGameUpdate, GameUpdate(..), Reducer, Req, _stateRef, runGameAff, runReducer)
import Game.Util (runStateWithRef)
import Prim.Row as Row
import Run (AFF, EFFECT, FProxy, Run, SProxy(..), case_, expand, interpret, lift, on, run, send)
import Run.Except (EXCEPT)
import Run.Reader (READER, askAt)
import Run.State (STATE)
