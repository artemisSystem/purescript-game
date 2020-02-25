{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "game"
, dependencies =
    [ "aff"
    , "canvas-action"
    , "console"
    , "datetime"
    , "effect"
    , "filterable"
    , "foldable-traversable"
    , "identity"
    , "js-timers"
    , "monad-loops"
    , "newtype"
    , "now"
    , "partial"
    , "polymorphic-vectors"
    , "prelude"
    , "psci-support"
    , "record"
    , "refs"
    , "tailrec"
    , "undefined"
    , "web-html"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/3ddyy/purescript-game.git"
}
