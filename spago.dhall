{ name = "game"
, dependencies =
  [ "aff"
  , "canvas-action"
  , "control"
  , "datetime"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "fork"
  , "js-timers"
  , "newtype"
  , "now"
  , "parallel"
  , "polymorphic-vectors"
  , "prelude"
  , "refs"
  , "run"
  , "tailrec"
  , "typelevel-prelude"
  , "variant"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/artemisSystem/purescript-game.git"
}
