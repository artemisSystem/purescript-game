module Game
  ( GameUpdate(..)
  , runUpdate

  , Reducer
  , mkReducer
  , runReducer
  , composeReducer
  , (>->)
  , identityReducer

  , Game
  , mkRunGame
  ) where


import Prelude

import Control.Apply (lift2)
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect, liftEffect)
import Prim.Row (class Union, class Nub)
import Run (Run)
import Run.Unsafe (Anything)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)


foreign import data Reducer ∷ Row (Type → Type) → Row (Type → Type) → Type

-- todo: maaayyyybe we can get away with using the union typeclass for the anything part
-- todo: maaayyyybe a way to have a more specific reducer for more customizability
-- | Create a `Reducer`. Note: doing `mkReducer $ f` will raise an
-- | `EscapedSkolem` error. Use parentheses or `do` instead of `$`.
mkReducer ∷
  ∀ extra req extra_req
  . Union extra req extra_req
  ⇒ (Run (Anything + extra_req) ~> Run (Anything + req))
  → Reducer extra req
mkReducer = unsafeCoerce

-- | Turn a `Reducer` into a function that acts on a specific effect row 
runReducer ∷
  ∀ update execIn nubExecIn extra req req_execIn a
  . Union execIn extra update
  ⇒ Union req execIn req_execIn
  ⇒ Nub execIn nubExecIn
  ⇒ Nub req_execIn nubExecIn
  ⇒ Reducer extra req
  → (Run update a → Run execIn a)
runReducer reducer update =
  (unsafeCoerce reducer ∷ Run update a → Run execIn a) update

-- | Compose two `Reducer`s left to right
composeReducer ∷
  ∀ extra1 extra2 extra3 req extra2r extra3r
  . Union extra1 extra2 extra3
  ⇒ Union extra2 req extra2r
  ⇒ Union extra3 req extra3r
  ⇒ Union extra2r extra1 extra3r
  ⇒ Reducer extra1 req → Reducer extra2 req → Reducer extra3 req
composeReducer r1 r2 = mkReducer (f1 >>> f2)
  where
    f1 ∷ ∀ a. Run (Anything extra3r) a → Run (Anything extra2r) a
    f1 = unsafeCoerce r1
    f2 ∷ ∀ a. Run (Anything extra2r) a → Run (Anything req) a
    f2 = unsafeCoerce r2

infixl 5 composeReducer as >->

-- | The identity `Reducer`. `runReducer identityReducer` is the same as
-- | `identity`.
identityReducer ∷ ∀ req. Reducer () req
identityReducer = mkReducer identity

-- todo: maybe it's possible to change it to "automatically apply the reducer"
newtype GameUpdate ∷
  Row (Type → Type) → Row (Type → Type) → Row (Type → Type) → Type → Type
newtype GameUpdate extra req execOut a =
  GameUpdate (Reducer extra req → Run execOut a)

derive instance Newtype (GameUpdate extra req execOut a) _

instance Functor (GameUpdate extra req execOut) where
  map = liftM1

instance Apply (GameUpdate extra req execOut) where
  apply = ap
 
instance Applicative (GameUpdate extra req execOut) where
  pure x = GameUpdate (\_ → pure x)

instance Bind (GameUpdate extra req execOut) where
  bind (GameUpdate m) f = GameUpdate \reducer → do
    m reducer >>= \a → case f a of GameUpdate b → b reducer

instance Monad (GameUpdate extra req execOut)

instance Semigroup a ⇒ Semigroup (GameUpdate extra req execOut a) where
  append = lift2 (<>)

instance Monoid a ⇒ Monoid (GameUpdate extra req execOut a) where
  mempty = pure mempty

instance MonadEffect (Run execOut) ⇒ MonadEffect (GameUpdate extra req execOut) where
  liftEffect f = GameUpdate \_ → liftEffect f

runUpdate ∷
  ∀ extra req execOut a
  . Reducer extra req
  → GameUpdate extra req execOut a
  → Run execOut a
runUpdate reducer (GameUpdate update) = update reducer

type Game ∷
  Row (Type → Type) → Row (Type → Type) → Row (Type → Type) → Type → Type
type Game extra req execOut a = Array (GameUpdate extra req execOut a)

-- | Make a function that can run a `Game` in `Run`
mkRunGame ∷
  ∀ extra req execOut interpreted a b
  . (Run execOut a → Run interpreted b)
  → (Array (Run interpreted b) → Run interpreted b)
  → (Reducer extra req → Game extra req execOut a → Run interpreted b)
mkRunGame interpret parallelize reducer updates = updates
  # map (runUpdate reducer)
  # map interpret
  # parallelize
