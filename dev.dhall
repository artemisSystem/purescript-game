let conf = ./spago.dhall

in conf //
  { dependencies = conf.dependencies #
    [ "canvas-action"
    , "console"
    , "psci-support"
    ]
  , sources = conf.sources # [ "examples/**/*.purs" ]
  }