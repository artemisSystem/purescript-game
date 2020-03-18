module Game where

import Prelude

import Prim.Row (class Union)
import Run (Run)
import Unsafe.Coerce (unsafeCoerce)


type GameUpdateF (update :: # Type) (execIn :: # Type) (execOut :: # Type) =
  { update :: Run update Unit
  , exec   :: Run execIn Unit -> Run execOut Unit
  }

data GameUpdate (extra :: # Type) (req :: # Type) (execOut :: # Type)

mkUpdate
  :: forall update execIn extra req execOut
   . Union execIn extra update => Union req execIn execIn
  => GameUpdateF update execIn execOut -> GameUpdate extra req execOut
mkUpdate = unsafeCoerce

runUpdate
  :: forall update execIn extra req execOut
   . Union extra execIn update
  => Union req execIn execIn
  => (Run update Unit -> Run execIn Unit)
  -> GameUpdate extra req execOut
  -> Run execOut Unit
runUpdate reduce gameUpdate = case coerce gameUpdate of
  { update, exec } -> exec (reduce update)
  where
    coerce :: GameUpdate extra req execOut -> GameUpdateF update execIn execOut
    coerce = unsafeCoerce


type Game
  (extra :: # Type)
  (req :: # Type)
  (execOut :: # Type)
  = Array (GameUpdate extra req execOut)

foreign import mkRunGameImpl
  :: (  forall update execIn extra req execOut
     .  Union extra execIn update
     => Union req execIn execIn
     => (Run update Unit -> Run execIn Unit)
     -> GameUpdate extra req execOut
     -> Run execOut Unit
     ) -- This first part of the type (above) is the type of `runUpdate`
  -> (  forall extra req execOut interpreted a
     .  (Run execOut Unit -> Run interpreted a)
     -> (forall b. Array (Run interpreted b) -> Run interpreted b)
     -> (  forall update execIn r
        .  Union req r execIn => Union extra execIn update
        => Run update Unit -> Run execIn Unit
        )
     -> Game extra req execOut
     -> Run interpreted a
     ) -- And this second half is the type of `mkRunGame`


mkRunGame
  :: forall extra req execOut interpreted a
   . (Run execOut Unit -> Run interpreted a)
  -> (forall b. Array (Run interpreted b) -> Run interpreted b)
  -> (  forall update execIn r
     .  Union req r execIn => Union extra execIn update
     => Run update Unit -> Run execIn Unit
     )
  -> Game extra req execOut
  -> Run interpreted a
mkRunGame = mkRunGameImpl runUpdate