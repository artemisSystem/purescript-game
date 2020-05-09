# purescript-game

## Documentation

### TODO: Rewrite the readme

Talk about:

- the smaller `req` is, the less requirement you set for the updates you can
  use, but `extra` needs to be a row that can be reduced to `req`

OLD:

This is a package for running a game loop in a browser in purescript using
`window.requestAnimationFrame`.

It has a type alias called `Game` which is a record containing various functions
needed to run your game loop.

It looks like this:

```purescript
type Game state return =
  { init    :: Effect state
  , update  :: Seconds -> state -> Effect state
  , display :: state -> Effect Unit
  , end     :: state -> Effect (Maybe (Either Error return))
  , events  :: List (GameEvent state)
  }
```

`state` is the type of the state used in your game. This will most likely be a
record type containing all the different values your game needs.

`return` is the type of the value that will be returned by the `Aff` your game
is run in, if and when it terminates.

`init :: Effect state` is the initial state of your game. It is wrapped in
`Effect` so that it is possible for the initial state to depend on random
number generation, database queries and other things.

`update :: Seconds -> state -> Effect state` is the state update function that
will be run every frame. It takes a value of type `Seconds`, which is the time
since the last frame, and a value of type `state`, which is the state of the
game before the current frame started. It needs to return a value of type
`Effect state`, where the containted `state` will be the state of the game after
the frame.

`display :: state -> Effect Unit` is the function that is used to render your
game. Given the state of the game, it needs to return an `Effect Unit`, which
should display the game in some way. This is usually drawing on a HTML5 canvas,
manipulating the DOM, or logging to the console. This function is run right
after `update`.

`end :: state -> Effect (Maybe (Either Error return))` is the function that is
used to determine whether to terminate the game loop. It is run after every
frame. Given the current state, if it returns `Nothing`, the game loop will keep
running for another frame. If it returns a `Just`, the game loop will end. If
the inner value is an `Error`, the `Aff` will error, and if the inner value is
of type `return`, it will resolve the `Aff` with that value.

`events :: List (GameEvent state)` is a list of events watched by the game.
The type alias `GameEvent` is used to represent these events. It looks like
this:

```purescript
type GameEvent state =
  { eventType  :: EventType
  , target     :: EventTarget
  , update     :: Event -> state -> Effect state
  , useCapture :: Boolean
  }
```

`eventType :: EventType` is the type of the event, see [`EventType`](https://pursuit.purescript.org/packages/purescript-web-events/2.0.1/docs/Web.Event.Event#t:EventType).

`target :: EventTarget` is the event target, see [`EventTarget`](https://pursuit.purescript.org/packages/purescript-web-events/2.0.1/docs/Web.Event.Internal.Types#t:EventTarget).

`update :: Event -> state -> Effect state` is the function that gets run when
the event occurs. It takes the `Event` object and the current state, and returns
a new state, wrapped in `Effect`.

`useCapture :: Boolean` specifies whether to use capture, see
[`EventTarget.addEventListener`](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener).

Use `game` to create an `Aff` from a `Game` value. From there, you can use
`launchAff_` from [`Effect.Aff`](https://pursuit.purescript.org/packages/purescript-aff/5.1.2/docs/Effect.Aff#v:launchAff_)
to run the `Aff` in an `Effect`. Other ways to run an `Aff` and more info can
be found [here](https://pursuit.purescript.org/packages/purescript-aff/5.1.2).

### Reference

Module reference is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-game).
