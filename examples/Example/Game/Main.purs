module Example.Game.Main where

import Prelude

import Data.Foldable (fold)
import Data.Monoid.Additive (Additive(..))
import Data.Traversable (sequence)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Game (Game, GameUpdate(..), Reducer, identityReducer, mkReducer, mkRunGame, runReducer)
import Run (EFFECT, Run, interpret, match, runBaseEffect)
import Run.Reader (Reader, askAt, runReaderAt)
import Run.State (STATE, evalStateAt, _state, get, modify, put)
import Run.Writer (WRITER, Writer(..), foldWriterAt, tell, tellAt)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type ExecOut = WRITER String + ()

type Req ∷ Row (Type → Type)
type Req = ()

type Interpreted = EFFECT + ()

_end ∷ Proxy "end"
_end = Proxy

interpretGame ∷ ∀ a. Run ExecOut a → Run Interpreted a
interpretGame execOut = execOut
  # interpret (match { writer: \(Writer w a) → log w $> a })

parallelize
  ∷ ∀ a. Monoid a ⇒ Array (Run Interpreted a) → Run Interpreted a
parallelize = sequence >>> map fold

runGame ∷
  ∀ extra a
  . Monoid a
  ⇒ Reducer extra Req
  → Game extra Req ExecOut a
  → Run Interpreted a
runGame = mkRunGame interpretGame parallelize


type Read5In r = WRITER String + (five ∷ Reader Int | r)

_five ∷ Proxy "five"
_five = Proxy

read5Update ∷
  ∀ extra a
  . Run (Read5In extra) a
  → GameUpdate extra Req ExecOut a
read5Update update = GameUpdate \reducer →
  runReaderAt _five 5 (runReducer reducer update)

type StateIn r = WRITER String + STATE Int + r

stateUpdate ∷
  ∀ extra a
  . Run (StateIn extra) a
  → GameUpdate extra Req ExecOut a
stateUpdate update = GameUpdate \reducer →
  evalStateAt _state 26 (runReducer reducer update)


type Extra1 = (six ∷ Reader Int)

_six ∷ Proxy "six"
_six = Proxy

game1 ∷ Game Extra1 Req ExecOut String
game1 =
  [ stateUpdate do
      tell "\nstate update in game1:"
      init ← get
      tell (show init)
      tell (show $ init + 1)
      six ← askAt _six
      put six
      new ← get
      tell (show new)
      pure "Result from stateUpdate in game1"
  , read5Update do
      tell "\nread5 update in game1:"
      five ← askAt _five
      tell (show five)
      tell (show $ five + 1)
      pure (" - " <> show five)
  ]


type Extra2 = (void ∷ Writer String)

_void ∷ Proxy "void"
_void = Proxy

game2 ∷ Game Extra2 Req ExecOut (Additive Int)
game2 =
  [ stateUpdate do
      tell "\nstate update in game2:"
      tellAt _void "I AM SHOUTING INTO THE VOID"
      init ← get
      tellAt _void do "i wonder what this number is: " <> show init
      tell "NOW I'M SHOUTING IN YOUR CONSOLE"
      modify (_ + 11)
      get <#> Additive
  , GameUpdate \reducer → runReducer reducer do
      tell "\nsecond update in game2:"
      tell
         $ "This update uses an update template that is defined right where "
        <> "it's used. It just runs the reducer that is global to the game, "
        <> "and doesn't interpret any effects in addition to that, meaning "
        <> "that there are no effects that are unique to it. The `Run` that "
        <> "it's passed only has access to the effects from the game's extra "
        <> "and execOut (like this writer, which comes from the execOut)."
      pure (Additive 5)
  ]

game3 ∷ Game () Req ExecOut Unit
game3 =
  [ read5Update do
      tell "\nread5 update in game3:"
      five ← askAt _five
      tell (show five)
      tell (show $ five * five)
  ]

-- todo: move these reducers next to the `ExtraX` definitions
main ∷ Effect Unit
main = runBaseEffect do
  result1 ← runGame (mkReducer do runReaderAt _six 6) game1
  log do "result of game 1: " <> show result1

  result2 ← runGame (mkReducer do foldWriterAt _void const "" >>> map snd) game2
  log do "result of game 2: " <> show result2

  result3 ← runGame identityReducer game3
  log do "result of game 3: " <> show result3
