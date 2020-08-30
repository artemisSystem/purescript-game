# purescript-game

## Installation

```sh
spago install game
```

## Documentation

- [Reference](#reference)
- [The `Game` module](#the-game-module)
  - [`Reducer`](#reducer)
  - [`GameUpdate`](#gameupdate)
  - [`Game`](#game)
- [Using `AffGame`](#using-affgame)

### Reference

Module reference is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-game).

### The `Game` module

This section explains in detail how `Game` works and what it can do. If you just
want to learn about `AffGame`, you can skip forward.

#### `Reducer`

`Reducer` is for describing a reduction of a `Run` effect row. Its first
argument `extra` contains the effects that will be removed, and its second
argument `req` contains the effects that must be present in the row to perform
the reduction. So, having a `Reducer extra req` means that you can interpret all
the effects in `extra` either purely or in terms of any of the effects in `req`.
`mkReducer` is a function you can use to construct a reducer, where you provide
a function `Run (Anything + extra_req) ~> Run (Anything + req)` where
`extra_req` is the union of `extra` and `req`. See the top-level docs in
`Run.Unsafe` for a more in-depth explanation of `Anything`.

There is provided an `identityReducer`, which is a reducer that doesn't reduce
any effects, useful for when your `extra` is an empty row. There is also
`composeReducer` (infix version `(>->)`), which composes two reducers left to
right.

#### `GameUpdate`

`GameUpdate` is a type constructor that takes four arguments: `extra ∷ # Type`,
`req ∷ # Type`, `execOut ∷ # Type` and `a ∷ Type`. It's a newtype around
`Reducer extra req → Run execOut a`. In essence, it's a `Run` action that can
run effects from `extra`, provided all effects in `req` are present.

If you want to make a template function to construct a `GameUpdate`, you can
structure it like this:

- Have `update` be the union of `execIn` and `extra`, and have the caller
supply a `Run update a` that you convert to a `Run execIn a` using the reducer.
- Turn `Run execIn a` into `Run execOut a`, reducing the specific effects for
your update template into the more general effects supported globally in the
`Game`.
- `execIn` will in most cases be a superset of `execOut`

Note that it is usually recommended to make your own row type like this:

```purescript
type MyUpdateExecIn r =
  ( {- your execIn effects go in here, looking like `name ∷ EFFECT_NAME` -}
  | r)`
```

Where you omit the `Union` constraint and the `update` type variable, and use
`MyUpdateExecIn extra` instead of `update`. You can find an explanation to why
this may be needed [here](https://github.com/purescript/purescript/issues/3242).

#### `Game`

A `Game` is simply an `Array` of `GameUpdate`s.

The function `mkRunGame` lets you create a function that will run the game. It
takes two functions as input:

- `interpret ∷ Run execOut a → Run interpreted b`  
This function decides how all the updates will get interpreted individually.
- `parallelize ∷ Array (Run interpreted b) → Run interpreted b`  
This function decides how all the updates will be combined together. It is
called `parallelize` internally because it will usually run all the updates in
parallel. This is what `mkAffGame` does.

After calling `mkRunGame` with these two functions, you will have a `Reducer
extra req → Game extra req execOut a → Run interpreted b`. You can then pass in
a `Reducer` and a `Game`, and it will be run in `Run` with the `interpreted` row
of effects.

### Using `AffGame`
