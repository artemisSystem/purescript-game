{-
| See `Game.mkReducer` for a use case of this module. In essence, it allows you
| to talk about open rows in `Run` or `VariantF` without them actually being
| open rows. This can help get around some issues with the type system. For this
| to work, the `("💣" ∷ Unsafe)` row must represent a row that can be of any
| length and contain any fields. Therefore, it is unsafe to interact with this
| field, which is why the bomb emoji is used. In other words, when this module
| "is in play", any pattern match function or similar on a `VariantF` matching
| on the `"💣"` field is unsafe. It is up to you to handle everything,
| this module just defines the types you will need.
-}
module Run.Unsafe where

import Prelude

import Safe.Coerce (coerce)

data Unsafe ∷ ∀ k. k → Type
data Unsafe a

instance Functor Unsafe where
  map ∷ ∀ a b. (a → b) → Unsafe a → Unsafe b
  map _ = coerce

type Anything ∷ ∀ k. Row (k → Type) → Row (k → Type)
type Anything r = ("💣" ∷ Unsafe | r )
