{-
| See `Game.mkReducer` for a use case of this module. In essence, it allows you
| to talk about open rows in `Run` or `VariantF` without them actually being
| open rows. This can help get around some issues with the type system. For this
| to work, the `("💣" :: UNSAFE)` row must represent a row that can be of any
| length and contain any fields. Therefore, it is unsafe to interact with this
| field, which is why the bomb emoji is used. In other words, when this module
| is in play, using any pattern match function or similar on a `VariantF`, and
| matching on the `"💣"` field, is unsafe. It is up to you to handle everything,
| this module just defines the types you will need.
-}
module Run.Unsafe where

import Prelude

import Run (FProxy)
import Unsafe.Coerce (unsafeCoerce)


data Unsafe a

instance functorUnsafe :: Functor Unsafe where
  map :: forall a b. (a -> b) -> Unsafe a -> Unsafe b
  map _ = unsafeCoerce

type UNSAFE = FProxy Unsafe

type Anything r = ("💣" :: UNSAFE | r )