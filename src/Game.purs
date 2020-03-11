module Game where

import Prelude

import Control.Monad.Free (Free, foldFree, hoistFree)
import Data.Newtype (class Newtype, over, over2, un)
import Prim.Row (class Union)
import Record (modify)
import Run (Run, SProxy(..), expand)
import Unsafe.Coerce

-- ge = effect, canvas
-- r = effect, end
-- r' = effect, canvas, end
-- ex = effect, aff
type GameUpdateF (ge :: # Type) (r :: # Type) (r' :: # Type) (ex :: # Type) a =
  { update :: Run r' Unit
  , exec   :: Run r Unit -> Run ex a
  }

-- mge = effect
-- ge = effect
-- ge2 = effect, canvas
-- r = effect, end
-- Equivalent to a GameUpdateF ge2 r r'2 ex a
type GameUpdateMapF
  (mge :: # Type)
  (ge :: # Type)
  (ge2 :: # Type)
  (r :: # Type)
  (r'1 :: # Type)
  (r'2 :: # Type)
  (ex :: # Type)
  a
  = { gameUpdate :: GameUpdate mge ge ex a
    , mapper     :: Run r'1 Unit -> Run r'2 Unit
    }

-- | GameUpdate is an existential type, saying that for a certain set of `mge`
  -- | (minimum game effects), `ge` (current game effects), `ex` ("executor"
  -- | effects), and `a` (game return type), it contains a function `exec`, which
  -- | "runs" an `Run r Unit` in a `Run ex a`, and a computation
  -- | `update :: Run r' Unit`. It also specifies the following relations:
  -- |
  -- | - `mge` is a subset of `r`, that is, every field in `mge` is also in `r`
  -- | - The union of `ge` and `r` is `r'`
  -- |
  -- | This means that if `mge` and `ge` are the same, so are `r` and `r'`, and we
  -- | can run `update` with `exec` to get out our `Run ex a`. While `mge` and
  -- | `ge` are not the same, the only thing that can be done is to reduce `ge` to
  -- | `mge`. The reason they would be different is that when adding `GameUpdate`s
  -- | to a `Game`, you might want to add multiple updates which for example have
  -- | the `CANVAS` effect (`CANVAS` would then be in `ge`, and not in `mge`).
  -- | Then you could use a function that can reduce `ge` on the whole `Game` to
  -- | collapse `CANVAS` into `EFFECT` by providing it a `Context2D`. That way,
  -- | your `GameUpdate`s' `update` computations can all use the `CANVAS` effect,
  -- | while all the `exec` functions only need to know how to handle `EFFECT`.
data GameUpdate mge ge ex a
  = GU
      (  forall res
      .  (  forall r r_ r'
          . Union mge r_ r => Union ge r r'
         => GameUpdateF ge r r' ex a -> res )
      -> res )
  | MGU
      (  forall res
      .  (  forall r r_ r'1 r'2 ge2
          . Union mge r_ r => Union ge r r'1 => Union ge2 r r'2
         => GameUpdateMapF mge ge ge2 r r'1 r'2 ex a -> res )
      -> res )

mkUpdate
  :: forall mge ge r r' r_ ex a
   . Union mge r_ r => Union ge r r'
  => Run r' Unit -> (Run r Unit -> Run ex a) -> GameUpdate mge ge ex a
mkUpdate update exec = GU (_ $ { update, exec })

runUpdateWith
  :: forall mge ge r r'1 r'2 r_ ex a
   . Union mge r_ r => Union ge r r'1 => Union mge r r'2
  => (Run r'1 Unit -> Run r'2 Unit)
  -> GameUpdate mge ge ex a
  -> Run ex a
runUpdateWith f (GU u) = u \{ update, exec } -> exec (coerce2 $ f $ coerce1 update)
  where
    coerce1 :: _ -> Run r'1 Unit
    coerce1 = unsafeCoerce
    coerce2 :: Run r'2 Unit -> _
    coerce2 = unsafeCoerce
runUpdateWith f (MGU u) = u \{ gameUpdate, mapper } ->
  runUpdateWith (f <<< mapper) gameUpdate

-- runUpdate :: forall ge ex a. GameUpdate ge ge ex a -> Run ex a
-- runUpdate = runUpdateWith identity

-- mapUpdate
--   :: forall mge ge1 ge2 r r'1 r'2 r_ ex a
--    . Union mge r_ r => Union ge1 r r'1 => Union ge2 r r'2
--   => (Run r'1 Unit -> Run r'2 Unit)
--   -> GameUpdate mge ge1 ex a
--   -> GameUpdate mge ge2 ex a
-- mapUpdate f (GameUpdate u) = u
--   \{ update, exec } -> mkUpdate (coerce2 do f do coerce1 update) (coerce3 exec)
--   where
--     coerce1 :: _ -> Run r'1 Unit
--     coerce1 = unsafeCoerce
--     coerce2 :: forall a. Union ge2 r__ a => Run r'2 Unit -> Run a Unit
--     coerce2 = unsafeCoerce
--     coerce3 :: _ -> _
--     coerce3 = unsafeCoerce


newtype GameF mge ge ex a = GameF (Array (GameUpdate mge ge ex a))

derive instance newtypeGameF :: Newtype (GameF mge ge ex a) _

instance semigroupGameF :: Semigroup (GameF mge ge ex a) where
  append = over2 GameF append

instance monoidGameF :: Monoid (GameF mge ge ex a) where
  mempty = GameF []

type Game mge ge ex = Free (GameF mge ge ex)

-- TODO: fromUpdates :: Array (GameUpdate mge ge ex a) -> Game mge ge ex a

-- mkRunGame
--   :: forall ge ex ig a
--    . (Run ex ~> Run ig)
--   -> (forall b. Array (Run ig b) -> Run ig b)
--   -> Game ge ge ex a
--   -> Run ig a
-- mkRunGame runExec parallelize =
--   foldFree do un GameF >>> map (runUpdate >>> runExec) >>> parallelize

-- | Map over all the `GameUpdate`s in a `Game`
mapGame
  :: forall mge1 mge2 ge1 ge2 l1 l2
   . (GameUpdate mge1 ge1 l1 ~> GameUpdate mge2 ge2 l2)
  -> (Game mge1 ge1 l1 ~> Game mge2 ge2 l2)
mapGame f = hoistFree do over GameF (_ <#> f)

-- | Change the effects that are used in a `Game`
-- mapEffects
--   :: forall ge1 ge2 mge s a
--    . (Run ge1 Unit -> Run ge2 Unit) -> Game mge ge1 s a -> Game mge ge2 s a
-- mapEffects = mapGame <<< ?mapUpdate