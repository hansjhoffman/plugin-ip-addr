let config = ./spago.dhall

in config // {
  sources = config.sources # [ "test/**/*.purs" ],
  dependencies = config.dependencies # [ "aff", "effect", "gen", "spec", "quickcheck" ]
}
