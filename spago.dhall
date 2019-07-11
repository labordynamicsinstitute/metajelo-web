{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "purescript-metajelo-web"
, dependencies =
    [ "prelude"
    , "concur-react"
    , "email-validate"
    , "foreign-object"
    , "metajelo"
    , "profunctor"
    , "stringutils"
    , "url-validator"
    ]
, packages =
    ./packages.dhall
}
