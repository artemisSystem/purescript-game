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
    , "foldable-traversable"
    , "identity"
    , "newtype"
    , "now"
    , "partial"
    , "polymorphic-vectors"
    , "prelude"
    , "psci-support"
    , "refs"
    , "undefined"
    , "web-html"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
