{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "purescript-metajelo-web-test"
, dependencies =
    [ "debug", "foreign", "metajelo-web", "test-unit" ]
, packages =
    (../packages.dhall) //
    { metajelo-web =
        { repo = "../app"
        , version = ""
        , dependencies = (../app/spago.dhall).dependencies
        }
    }

}
