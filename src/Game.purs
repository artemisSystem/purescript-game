module Game
  ( GameUpdate
  , mkUpdate
  , runUpdate
  , Reducer
  , mkReducer
  , Game
  , mkRunGame
  ) where


import Prelude

import Prim.Row (class Union, class Nub)
import Run (Run)
import Run.Anything (Anything)
import Unsafe.Coerce (unsafeCoerce)


type GameUpdateF (update :: # Type) (execIn :: # Type) (execOut :: # Type) =
  { update :: Run update Unit
  , exec   :: Run execIn Unit -> Run execOut Unit
  }

data GameUpdate (extra :: # Type) (req :: # Type) (execOut :: # Type)

mkUpdate
  :: forall update execIn nubExecIn extra req req_execIn execOut
   . Union execIn extra update
  => Union req execIn req_execIn
  => Nub execIn nubExecIn
  => Nub req_execIn nubExecIn
  => (Run execIn Unit -> Run execOut Unit)
  -> Run update Unit
  -> GameUpdate extra req execOut
mkUpdate exec update = unsafeCoerce
  ({ update, exec } :: GameUpdateF update execIn execOut)

runUpdate
  :: forall extra req execOut
   . Reducer extra req
  -> GameUpdate extra req execOut
  -> Run execOut Unit
runUpdate reducer gameUpdate = case coerceUpdate gameUpdate of
  { update, exec } -> exec (coerceReducer reducer update)
  where
    -- These uses of `Anything` aren't technically correct, but if they were to
    -- be used correctly, I'd have to introduce another type variable that is
    -- the union of `extra` and `req`. This leads to inference issues, so
    -- instead I'm using `Anything` like this, which works just fine, since it's
    -- never exposed anywhere and is just so that the `Reducer` and `GameUpdate`
    -- can be used. Another option would have been to use the FFI to avoid the
    -- typechecking altogether.
    coerceUpdate
      :: GameUpdate extra req execOut
      -> GameUpdateF (Anything ()) (Anything ()) execOut
    coerceUpdate = unsafeCoerce
    coerceReducer
      :: Reducer extra req
      -> (Run (Anything ()) Unit -> Run (Anything ()) Unit)
    coerceReducer = unsafeCoerce


data Reducer (extra :: # Type) (req :: # Type)

mkReducer
  :: forall extra req extra_req
   . Union extra req extra_req
  => (Run (Anything extra_req) Unit -> Run (Anything req) Unit)
  -> Reducer extra req
mkReducer = unsafeCoerce


type Game
  (extra :: # Type)
  (req :: # Type)
  (execOut :: # Type)
  = Array (GameUpdate extra req execOut)

mkRunGame
  :: forall extra req execOut interpreted a
   . (Run execOut Unit -> Run interpreted a)
  -> (Array (Run interpreted a) -> Run interpreted a)
  -> Reducer extra req
  -> Game extra req execOut
  -> Run interpreted a
mkRunGame interpret parallelize reducer updates = updates
  # map (runUpdate reducer)
  # map interpret
  # parallelize
