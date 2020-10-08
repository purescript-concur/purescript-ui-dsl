{ name = "ui-dsl"
, dependencies =
  [ "arrays"
  , "record" -- has ffi. But depended upon by heterogenous anyways
  , "heterogeneous"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
