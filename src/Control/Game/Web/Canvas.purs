module Control.Game.Web.Canvas where

import Prelude

import Control.Game (class ToGame, EffectUpdate, toGame)
import Control.Game.Web (GameEvent, WebGame(..))
import Control.Game.Web.Util (qSel)
import Control.Monad.Error.Class (throwError)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Time.Duration (Seconds)
import Data.Traversable (for)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Graphics.Canvas as C
import Graphics.CanvasAction (CanvasAction, Context2D, runAction)
import Record (modify)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ParentNode (QuerySelector)
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLCanvasElement (fromElement, HTMLCanvasElement)


type CanvasUpdate s a =
  { step    :: s -> Effect s
  , render  :: s -> CanvasAction
  , resolve :: s -> Effect (Maybe a)
  }

toEffectUpdate :: forall s a. Context2D -> CanvasUpdate s a -> EffectUpdate s a
toEffectUpdate ctx = modify (SProxy :: _ "render") (map $ runAction ctx)


newtype CanvasGameEvent s a = CanvasGameEvent
  { eventType  :: EventType
  , target     :: EventTarget
  , update     :: Event -> CanvasUpdate s a
  , useCapture :: Boolean
  }

derive instance newtypeCanvasGameEvent :: Newtype (CanvasGameEvent s a) _

toGameEvent :: forall s a. Context2D -> CanvasGameEvent s a -> GameEvent s a
toGameEvent ctx = over CanvasGameEvent
  $ modify (SProxy :: _ "update") (map $ toEffectUpdate ctx)


newtype CanvasGame s a = CanvasGame
  { canvas :: QuerySelector
  , init   :: Aff s
  , setup  :: CanvasAction
  , frames :: Seconds -> CanvasUpdate s a
  , events :: List (CanvasGameEvent s a)
  }

derive instance newtypeCanvasGame :: Newtype (CanvasGame s a) _

instance toGameCanvasGame :: ToGame s a (CanvasGame s a) where
  toGame (CanvasGame { canvas, init, setup, frames, events }) = do
    ctx <- liftEffect getCtx >>= case _ of
      Nothing -> throwError (error "The canvas for canvasGame was not found.")
      Just ctx -> pure ctx
    toGame $ WebGame
      { init: init <* liftEffect (runAction ctx setup)
      , frames: frames <#> toEffectUpdate ctx
      , events: events <#> toGameEvent ctx
      }
    where
      toCanvasElement :: HTMLCanvasElement -> C.CanvasElement
      toCanvasElement = unsafeCoerce
      getCtx = do
        mCanv <- qSel canvas
          <#> (=<<) fromElement
          <#> map toCanvasElement
        for mCanv C.getContext2D