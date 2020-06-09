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

-- TODO: possibly express this in terms some of these functions (or just exec? idk) having access to the reducer.
-- makes for much freer uses of game updates
type GameUpdateF (update :: # Type) (execIn :: # Type) (execOut :: # Type) a =
  { init   :: Run update a
  , update :: a -> Run update a
  , exec   :: Run execIn a -> (a -> Run execIn a) -> Run execOut Unit
  }

data GameUpdate (extra :: # Type) (req :: # Type) (execOut :: # Type)

mkUpdate
  :: forall update execIn nubExecIn extra req req_execIn execOut a
   . Union execIn extra update
  => Union req execIn req_execIn
  => Nub execIn nubExecIn
  => Nub req_execIn nubExecIn
  => (Run execIn a -> (a -> Run execIn a) -> Run execOut Unit)
  -> Run update a
  -> (a -> Run update a)
  -> GameUpdate extra req execOut
mkUpdate exec init update = unsafeCoerce
  ({ init, update, exec } :: GameUpdateF update execIn execOut a)

mkUpdate'
  :: forall update execIn nubExecIn extra req req_execIn execOut
   . Union execIn extra update
  => Union req execIn req_execIn
  => Nub execIn nubExecIn
  => Nub req_execIn nubExecIn
  => (Run execIn Unit -> Run execOut Unit)
  -> Run update Unit
  -> GameUpdate extra req execOut
mkUpdate' exec update = mkUpdate exec' (pure unit) (const update)
  where exec' _ f = exec (f unit)

runUpdate
  :: forall extra req execOut
   . Reducer extra req
  -> GameUpdate extra req execOut
  -> Run execOut Unit
runUpdate reducer gameUpdate = case coerceUpdate gameUpdate of
  { init, update, exec } -> exec (reduce init) (reduce <$> update)
  where
    coerceUpdate
      :: GameUpdate extra req execOut
      -> GameUpdateF (Anything ()) (Anything ()) execOut Void
    coerceUpdate = unsafeCoerce
    coerceReducer
      :: Reducer extra req
      -> (Run (Anything ()) Void -> Run (Anything ()) Void)
    coerceReducer = unsafeCoerce
    reduce = coerceReducer reducer


data Reducer (extra :: # Type) (req :: # Type)

-- need to use `do`, not `$` (EscapedSkolem)
mkReducer
  :: forall extra req extra_req
   . Union extra req extra_req
  => (forall a. Run (Anything extra_req) a -> Run (Anything req) a)
  -> Reducer extra req
mkReducer = unsafeCoerce


type Game
  (extra :: # Type)
  (req :: # Type)
  (execOut :: # Type)
  = Array (GameUpdate extra req execOut)

mkRunGame
  :: forall extra req execOut interpreted a
    -- TODO: have a specified value a (current a becomes b) instead of Unit
   . (Run execOut Unit -> Run interpreted a)
  -> (Array (Run interpreted a) -> Run interpreted a)
  -> Reducer extra req
  -> Game extra req execOut
  -> Run interpreted a
mkRunGame interpret parallelize reducer updates = updates
  # map (runUpdate reducer)
  # map interpret
  # parallelize
