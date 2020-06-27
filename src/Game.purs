module Game
  ( GameUpdate(..)
  , runUpdate
  , Reducer
  , mkReducer
  , runReducer
  , Game
  , mkRunGame
  ) where


import Prelude

import Prim.Row (class Union, class Nub)
import Run (Run)
import Run.Unsafe (Anything)
import Unsafe.Coerce (unsafeCoerce)


newtype GameUpdate (extra ∷ # Type) (req ∷ # Type) (execOut ∷ # Type) a =
  GameUpdate (Reducer extra req → Run execOut a)

runUpdate ∷
  ∀ extra req execOut a
  . Reducer extra req
  → GameUpdate extra req execOut a
  → Run execOut a
runUpdate reducer (GameUpdate update) = update reducer


data Reducer (extra ∷ # Type) (req ∷ # Type)

-- mention in docs: need to use `do`, not `$` (EscapedSkolem)
mkReducer ∷
  ∀ extra req extra_req
  . Union extra req extra_req
  ⇒ (Run (Anything extra_req) ~> Run (Anything req))
  → Reducer extra req
mkReducer = unsafeCoerce

runReducer ∷
  ∀ update execIn nubExecIn extra req req_execIn a
  . Union execIn extra update
  ⇒ Union req execIn req_execIn
  ⇒ Nub execIn nubExecIn
  ⇒ Nub req_execIn nubExecIn
  ⇒ Reducer extra req
  → Run update a
  → Run execIn a
runReducer reducer update =
  (unsafeCoerce reducer ∷ Run update a → Run execIn a) update


type Game
  (extra ∷ # Type)
  (req ∷ # Type)
  (execOut ∷ # Type)
  a
  = Array (GameUpdate extra req execOut a)

mkRunGame ∷
  ∀ extra req execOut interpreted a b
  . (Run execOut a → Run interpreted b)
  → (Array (Run interpreted b) → Run interpreted b)
  → Reducer extra req
  → Game extra req execOut a
  → Run interpreted b
mkRunGame interpret parallelize reducer updates = updates
  # map (runUpdate reducer)
  # map interpret
  # parallelize
