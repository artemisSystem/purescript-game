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
import Game.Aff (AffGame, Reducer, Req, runGameAff)
import Prim.Row as Row
import Run (AFF, EFFECT, FProxy, Run, SProxy(..), case_, interpret, lift, on, run, send)

type AFFGAME extra = FProxy (AffGame extra)

_affGame ∷ SProxy "affGame"
_affGame = SProxy

liftAffGame ∷ ∀ extra a r. AffGame extra a → Run (affGame ∷ AFFGAME extra | r) a
liftAffGame = liftAffGameAt _affGame

liftAffGameAt ∷
  ∀ t extra a r s
  . IsSymbol s
  ⇒ Row.Cons s (AFFGAME extra) t r
  ⇒ SProxy s
  → AffGame extra a
  → Run r a
liftAffGameAt = lift

runAffGame ∷
  ∀ extra r
  . Reducer extra Req
  → Run (aff ∷ AFF, affGame ∷ AFFGAME extra | r) ~> Run (aff ∷ AFF | r)
runAffGame = runAffGameAt (SProxy ∷ _ "aff") _affGame

runAffGameAt ∷
  ∀ aff affGame extra r0 r1 r2
  . IsSymbol aff
  ⇒ IsSymbol affGame
  ⇒ Row.Cons aff AFF r0 r1
  ⇒ Row.Cons affGame (AFFGAME extra) r1 r2
  ⇒ SProxy aff
  → SProxy affGame
  → Reducer extra Req
  → Run r2 ~> Run r1
runAffGameAt aff affGame reducer = interpret
  (on affGame (runGameAff reducer >>> lift aff) send)

-- | Runs a base `AffGame` effect
runBaseAffGame ∷ ∀ extra. Run (affGame ∷ AFFGAME extra) ~> AffGame extra
runBaseAffGame = runBaseAffGameAt _affGame

-- | Runs a base `AffGame` effect at the provided label
runBaseAffGameAt ∷
  ∀ extra s r
  . IsSymbol s ⇒ Row.Cons s (AFFGAME extra) () r
  ⇒ SProxy s → Run r ~> AffGame extra
runBaseAffGameAt p = run (case_ # on p identity)

-- | Runs base `AffGame`, `Aff` and `Effect` together as one effect
runBaseAffGame' ∷
  ∀ extra
  . Run (effect ∷ EFFECT, aff ∷ AFF, affGame ∷ AFFGAME extra) ~> AffGame extra
runBaseAffGame' = runBaseAffGameAt'
  (SProxy ∷ _ "effect")
  (SProxy ∷ _ "aff")
  _affGame

-- | Runs base `AffGame`, `Aff` and `Effect` together as one effect at the
-- | provided labels
runBaseAffGameAt' ∷
  ∀ effect aff affGame extra r1 r2 r3
  . IsSymbol effect
  ⇒ IsSymbol aff
  ⇒ IsSymbol affGame
  ⇒ Row.Cons effect   EFFECT         () r1
  ⇒ Row.Cons aff      AFF            r1 r2
  ⇒ Row.Cons affGame (AFFGAME extra) r2 r3
  ⇒ SProxy effect
  → SProxy aff
  → SProxy affGame
  → Run r3 ~> AffGame extra
runBaseAffGameAt' effect aff affGame = case_
  # on effect  Effect.liftEffect
  # on aff     Aff.liftAff
  # on affGame identity
  # run