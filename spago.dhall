{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "game"
, dependencies =
  [ "aff"
  , "canvas-action"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "fork"
  , "free"
  , "functors"
  , "identity"
  , "js-timers"
  , "monad-loops"
  , "newtype"
  , "now"
  , "parallel"
  , "partial"
  , "polymorphic-vectors"
  , "prelude"
  , "psci-support"
  , "record"
  , "record-extra"
  , "refs"
  , "run"
  , "tailrec"
  , "typelevel-prelude"
  , "undefined"
  , "variant"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/3ddyy/purescript-game.git"
}
