{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aff"
    , "aff-promise"
    , "console"
    , "dotenv"
    , "effect"
    , "filterable"
    , "naporitan"
    , "node-buffer"
    , "node-http"
    , "node-process"
    , "node-streams"
    , "psci-support"
    , "simple-json"
    , "test-unit"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
