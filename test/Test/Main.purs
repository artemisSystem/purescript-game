module Test.Main where

import Prelude

import Data.Foldable (fold)
import Data.Monoid.Additive (Additive(..))
import Data.Traversable (sequence)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Game (GameUpdate(..), Game, Reducer, mkRunGame, mkReducer, runReducer)
import Run (EFFECT, Run, SProxy(..), interpret, match, runBaseEffect)
import Run.Reader (READER, askAt, runReaderAt)
import Run.State (STATE, evalStateAt, _state, get, modify, put)
import Run.Writer (WRITER, Writer(..), foldWriterAt, tell, tellAt)

type ExecOut = (writer ∷ WRITER String)

type Req = ()

type Interpreted = (effect ∷ EFFECT)

_end ∷ SProxy "end"
_end = SProxy

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


type Read5In r = (writer ∷ WRITER String, five ∷ READER Int | r)

_five ∷ SProxy "five"
_five = SProxy

read5Update ∷
  ∀ extra a
  . Run (Read5In extra) a
  → GameUpdate extra Req ExecOut a
read5Update update = GameUpdate \reducer →
  runReaderAt _five 5 (runReducer reducer update)

type StateIn r = (writer ∷ WRITER String, state ∷ STATE Int | r)

-- TODO: for docs: mention that using `Union` is generally janky, and that
-- support for the `extra` effects should be baked in to the type with the
-- effects the update supports.
stateUpdate ∷
  ∀ extra a
  . Run (StateIn extra) a
  → GameUpdate extra Req ExecOut a
stateUpdate update = GameUpdate \reducer →
  evalStateAt _state 26 (runReducer reducer update)


type Extra1 = (six ∷ READER Int)

_six ∷ SProxy "six"
_six = SProxy

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


type Extra2 = (void ∷ WRITER String)

_void ∷ SProxy "void"
_void = SProxy

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

main ∷ Effect Unit
main = runBaseEffect do
  result1 ← runGame (mkReducer do runReaderAt _six 6) game1
  log do "result of game 1: " <> show result1

  result2 ← runGame (mkReducer do foldWriterAt _void const "" >>> map snd) game2
  log do "result of game 2: " <> show result2

  result3 ← runGame (mkReducer identity) game3
  log do "result of game 3: " <> show result3
