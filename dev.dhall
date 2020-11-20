let conf = ./spago.dhall

in conf //
  { dependencies = conf.dependencies # [ "console", "psci-support" ]
  , sources = conf.sources # [ "test/**/*.purs" ]
  }