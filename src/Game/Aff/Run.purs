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
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class as Aff
import Effect.Class as Effect
import Game.Aff (AffGame, Reducer, Req, runGameAff)
import Prim.Row as Row
import Run (EFFECT, Run, AFF, case_, interpret, lift, on, run, send)
import Run.Except (EXCEPT, Except, _except, rethrowAt)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type AFFGAME extra err r = (affGame ∷ AffGame extra err | r)

_affGame ∷ Proxy "affGame"
_affGame = Proxy

liftAffGame ∷ ∀ extra err a r. AffGame extra err a → Run (AFFGAME extra err + r) a
liftAffGame = liftAffGameAt _affGame

liftAffGameAt ∷
  ∀ t extra err a r s
  . IsSymbol s
  ⇒ Row.Cons s (AffGame extra err) t r
  ⇒ Proxy s
  → AffGame extra err a
  → Run r a
liftAffGameAt = lift

runAffGame ∷
  ∀ extra err r
  . Reducer extra (Req err)
  → Run (AFF + EXCEPT err + AFFGAME extra err + r) ~> Run (AFF + EXCEPT err + r)
runAffGame = runAffGameAt (Proxy ∷ _ "aff") _except _affGame

runAffGameAt ∷
  ∀ aff except affGame extra err ra rb r1 r2
  . IsSymbol aff
  ⇒ IsSymbol except
  ⇒ IsSymbol affGame
  ⇒ Row.Cons aff Aff ra r1
  ⇒ Row.Cons except (Except err) rb r1
  ⇒ Row.Cons affGame (AffGame extra err) r1 r2
  ⇒ Proxy aff
  → Proxy except
  → Proxy affGame
  → Reducer extra (Req err)
  → Run r2 ~> Run r1
runAffGameAt aff except affGame reducer = interpret
  (on affGame
    (runGameAff reducer >>> lift aff >=> rethrowAt except)
    send)

-- | Runs a base `AffGame` effect
runBaseAffGame ∷ ∀ extra err. Run (AFFGAME extra err + ()) ~> AffGame extra err
runBaseAffGame = runBaseAffGameAt _affGame

-- | Runs a base `AffGame` effect at the provided label
runBaseAffGameAt ∷
  ∀ extra err s r
  . IsSymbol s ⇒ Row.Cons s (AffGame extra err) () r
  ⇒ Proxy s → Run r ~> AffGame extra err
runBaseAffGameAt p = run (case_ # on p identity)

-- | Runs base `AffGame`, `Aff` and `Effect` together as one effect
runBaseAffGame' ∷
  ∀ extra err
  . Run (EFFECT + AFF + AFFGAME extra err + ()) ~> AffGame extra err
runBaseAffGame' = runBaseAffGameAt'
  (Proxy ∷ _ "effect")
  (Proxy ∷ _ "aff")
  _affGame

-- | Runs base `AffGame`, `Aff` and `Effect` together as one effect at the
-- | provided labels
runBaseAffGameAt' ∷
  ∀ effect aff affGame extra err r1 r2 r3
  . IsSymbol effect
  ⇒ IsSymbol aff
  ⇒ IsSymbol affGame
  ⇒ Row.Cons effect Effect () r1
  ⇒ Row.Cons aff Aff r1 r2
  ⇒ Row.Cons affGame (AffGame extra err) r2 r3
  ⇒ Proxy effect
  → Proxy aff
  → Proxy affGame
  → Run r3 ~> AffGame extra err
runBaseAffGameAt' effect aff affGame = case_
  # on effect  Effect.liftEffect
  # on aff     Aff.liftAff
  # on affGame identity
  # run