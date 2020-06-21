module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Game (GameUpdate, Game, Reducer, mkRunGame, mkUpdate', mkReducer)
import Prim.Row (class Union)
import Run (EFFECT, Run, SProxy(..), interpret, match, runBaseEffect)
import Run.Except (EXCEPT, runExceptAt, throwAt)
import Run.Reader (READER, askAt, runReaderAt)
import Run.State (STATE, evalStateAt, _state, get, modify, put)
import Run.Writer (WRITER, Writer(..), foldWriterAt, tell, tellAt)

type ExecOut a = (writer ∷ WRITER String, end ∷ EXCEPT a)

type Req = ()

type Interpreted = (effect ∷ EFFECT)

_end ∷ SProxy "end"
_end = SProxy

interpretGame ∷ ∀ a. Run (ExecOut a) Unit → Run Interpreted (Maybe a)
interpretGame execOut = execOut
  # runExceptAt _end
  # map case _ of
      Left a → Just a
      Right _ → Nothing
  # interpret (match { writer: \(Writer w a) → log w $> a })

parallelize
  ∷ ∀ a. Array (Run Interpreted (Maybe a)) → Run Interpreted (Maybe a)
parallelize = sequence >>> map oneOf

runGame ∷ ∀ extra a
   . Reducer extra Req
  → Game extra Req (ExecOut a)
  → Run Interpreted (Maybe a)
runGame = mkRunGame interpretGame parallelize


type Read5In a =
  (writer ∷ WRITER String, end ∷ EXCEPT a, five ∷ READER Int)

_five ∷ SProxy "five"
_five = SProxy

read5Update
  ∷ ∀ extra update a
   . Union (Read5In a) extra update
  ⇒ Run update Unit → GameUpdate extra Req (ExecOut a)
read5Update = mkUpdate' (runReaderAt _five 5)

type StateIn a =
  (writer ∷ WRITER String, end ∷ EXCEPT a, state ∷ STATE Int)

stateUpdate
  ∷ ∀ extra update a
   . Union (StateIn a) extra update
  ⇒ Run update Unit → GameUpdate extra Req (ExecOut a)
stateUpdate = mkUpdate' (evalStateAt _state 26)


type Extra1 = (six ∷ READER Int)

_six ∷ SProxy "six"
_six = SProxy

game1 ∷ Game Extra1 Req (ExecOut String)
game1 =
  [ stateUpdate do
      tell "\nstate update:"
      init ← get
      tell (show init)
      tell (show $ init + 1)
      six ← askAt _six
      put six
      new ← get
      tell (show new)
      throwAt _end "Result from stateUpdate in game1" ∷ _ Unit
      tell "not reached"
  , read5Update do
      tell "\nread5 update:"
      five ← askAt _five
      tell (show five)
      tell (show $ five + 1)
      tell
         $ "the result below (in code) will be ignored, because my "
        <> "`parallelize` function just takes the first result"
      throwAt _end "Result from read5Update in game1" ∷ _ Unit
      tell "this will not be reached either"
  ]


type Extra2 = (void ∷ WRITER String)

_void ∷ SProxy "void"
_void = SProxy

game2 ∷ Game Extra2 Req (ExecOut Int)
game2 =
  [ stateUpdate do
      tell "\nstate update in game2:"
      tellAt _void "I AM SHOUTING INTO THE VOID"
      init ← get
      tellAt _void do "i wonder what this number is: " <> show init
      tell "NOW I'M SHOUTING IN YOUR CONSOLE"
      modify (_ + 16)
      get >>= throwAt _end
  ]

game3 ∷ Game () Req (ExecOut Void)
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
