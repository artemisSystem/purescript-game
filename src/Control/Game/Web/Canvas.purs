module Control.Game.Web.Canvas where

import Prelude

import Control.Game (class ToUpdate, EffectUpdate, toEffect, toUpdate)
import Control.Game.Util (iterateM, iterateUntilM', newRef, nowSeconds, readRef, writeRef)
import Control.Game.Web
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (class Newtype, over, over2)
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..), snd)
import Data.Symbol
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Graphics.CanvasAction
import Partial.Unsafe (unsafePartial)
import Record
import Web.DOM.ParentNode
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener, removeEventListener)
import Web.HTML (window) as W
import Web.HTML.Window (requestAnimationFrame, cancelAnimationFrame) as W
import Web.HTML.Window (Window)


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
  , update :: Seconds -> CanvasUpdate s a
  , events :: List (CanvasGameEvent s a)
  }
