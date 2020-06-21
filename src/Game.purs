module Game
  ( GameUpdate
  , mkUpdate
  , mkUpdate'
  , runUpdate
  , Reducer
  , mkReducer
  , Game
  , mkRunGame
  ) where


import Prelude

import Prim.Row (class Union, class Nub)
import Run (Run)
import Run.Unsafe (Anything)
import Unsafe.Coerce (unsafeCoerce)

-- TODO: express this as (Reducer extra req → Run execOut a).
-- I might not need to make it an opaque type??? since reducer has the type arguments we want.
-- makes for much freer uses of game updates. don't need two versions of mkUpdate.
---- I feel like i'd lose some necesscary restrictions when making this non-opaque.
---- I was thinking i would mean i'd have to propagate the union constraint to
---- the functions that make the updates, but i think i already need to do that.
---- I need to think about req and execIn though, and how all that works.
---- The necessary changes won't be as trivial as i'd initially imagined. I might
---- still need to coerce the reducer.
-- See runReducer for updated info.
type GameUpdateF (update ∷ # Type) (execIn ∷ # Type) (execOut ∷ # Type) a =
  { init   ∷ Run update a
  , update ∷ a → Run update a
  , exec   ∷ Run execIn a → (a → Run execIn a) → Run execOut Unit
  }

data GameUpdate (extra ∷ # Type) (req ∷ # Type) (execOut ∷ # Type)

mkUpdate ∷
  ∀ update execIn nubExecIn extra req req_execIn execOut a
  . Union execIn extra update
  ⇒ Union req execIn req_execIn
  ⇒ Nub execIn nubExecIn
  ⇒ Nub req_execIn nubExecIn
  ⇒ (Run execIn a → (a → Run execIn a) → Run execOut Unit)
  → Run update a
  → (a → Run update a)
  → GameUpdate extra req execOut
mkUpdate exec init update = unsafeCoerce
  ({ init, update, exec } ∷ GameUpdateF update execIn execOut a)

mkUpdate' ∷
  ∀ update execIn nubExecIn extra req req_execIn execOut
  . Union execIn extra update
  ⇒ Union req execIn req_execIn
  ⇒ Nub execIn nubExecIn
  ⇒ Nub req_execIn nubExecIn
  ⇒ (Run execIn Unit → Run execOut Unit)
  → Run update Unit
  → GameUpdate extra req execOut
mkUpdate' exec update = mkUpdate exec' (pure unit) (const update)
  where exec' _ f = exec (f unit)

-- GameUpdate has `a` at the end, `Run execOut Unit` becomes `Run execOut a`
runUpdate ∷
  ∀ extra req execOut
  . Reducer extra req
  → GameUpdate extra req execOut
  → Run execOut Unit
runUpdate reducer gameUpdate = case coerceUpdate gameUpdate of
  { init, update, exec } → exec (reduce init) (reduce <$> update)
  where
    coerceUpdate
      ∷ GameUpdate extra req execOut
      → GameUpdateF (Anything ()) (Anything ()) execOut Void
    coerceUpdate = unsafeCoerce
    coerceReducer
      ∷ Reducer extra req
      → (Run (Anything ()) Void → Run (Anything ()) Void)
    coerceReducer = unsafeCoerce
    reduce = coerceReducer reducer


data Reducer (extra ∷ # Type) (req ∷ # Type)

-- mention in docs: need to use `do`, not `$` (EscapedSkolem)
mkReducer ∷
  ∀ extra req extra_req
  . Union extra req extra_req
  ⇒ (∀ a. Run (Anything extra_req) a → Run (Anything req) a)
  → Reducer extra req
mkReducer = unsafeCoerce

-- TODO: have a `runReducer` that applies the necesscary restrictions.
-- this is because the coercion that is currently happening in `runUpdate` will
-- with the new implementation lie in the hands of the update maker/constructor
type Game
  (extra ∷ # Type)
  (req ∷ # Type)
  (execOut ∷ # Type)
  = Array (GameUpdate extra req execOut)

mkRunGame ∷
  ∀ extra req execOut interpreted a
    -- TODO: have a specified value a (current a becomes b) instead of Unit.
    -- Would need to change GameUpdateF and GameUpdate to have a type parameter.
    -- AffGame would still use Unit for this type, since it terminates through
    -- except.
  . (Run execOut Unit → Run interpreted a)
  → (Array (Run interpreted a) → Run interpreted a)
  → Reducer extra req
  → Game extra req execOut
  → Run interpreted a
mkRunGame interpret parallelize reducer updates = updates
  # map (runUpdate reducer)
  # map interpret
  # parallelize
