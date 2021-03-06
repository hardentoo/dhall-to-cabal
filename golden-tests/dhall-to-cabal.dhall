    let empty-package = ./dhall/defaults/Package.dhall 

in  let TestTypes = constructors ./dhall/types/TestType.dhall 

in  let licenses = constructors ./dhall/types/License.dhall 

in  let extensions = constructors ./dhall/types/Extension.dhall 

in  let package =
            λ(package : Text)
          → λ(version-range : VersionRange)
          → { bounds = version-range, package = package }

in  let common-deps =
          { Cabal =
              package "Cabal" (majorBoundVersion [ +2, +0 ])
          , base =
              package "base" (majorBoundVersion [ +4, +10 ])
          , bytestring =
              package "bytestring" (majorBoundVersion [ +0, +10 ])
          , dhall =
              package "dhall" (majorBoundVersion [ +1, +9 ])
          , dhall-to-cabal =
              package "dhall-to-cabal" anyVersion
          , text =
              package "text" (majorBoundVersion [ +1, +2 ])
          }

in  let gitHub-project = ./dhall/GitHub-project.dhall 

in  let OS = ./dhall/types/OS.dhall 

in  let always = λ(config : ./dhall/types/Config.dhall ) → True

in    gitHub-project { owner = "ocharles", repo = "dhall-to-cabal" }
    ⫽ { executables =
          [ { executable =
                [ { body =
                        ./dhall/defaults/Executable.dhall 
                      ⫽ { build-depends =
                            [ common-deps.base
                            , common-deps.dhall-to-cabal
                            , package
                              "optparse-applicative"
                              ( unionVersionRanges
                                (majorBoundVersion [ +0, +13, +2 ])
                                (majorBoundVersion [ +0, +14 ])
                              )
                            , common-deps.text
                            , common-deps.dhall
                            , common-deps.Cabal
                            ]
                        , hs-source-dirs =
                            [ "exe" ]
                        , main-is =
                            "Main.hs"
                        , other-extensions =
                            [ extensions.NamedFieldPuns True ]
                        }
                  , guard =
                      always
                  }
                ]
            , name =
                "dhall-to-cabal"
            }
          ]
      , library =
          [ [ { body =
                    ./dhall/defaults/Library.dhall 
                  ⫽ { build-depends =
                        [ common-deps.base
                        , common-deps.Cabal
                        , common-deps.dhall
                        , common-deps.text
                        , common-deps.bytestring
                        , package "containers" (majorBoundVersion [ +0, +5 ])
                        , package "vector" (majorBoundVersion [ +0, +12 ])
                        , package "trifecta" (majorBoundVersion [ +1, +7 ])
                        , package "text-format" (majorBoundVersion [ +0, +3 ])
                        , package
                          "transformers"
                          (majorBoundVersion [ +0, +5, +2 ])
                        ]
                    , compiler-options =
                          ./dhall/defaults/CompilerOptions.dhall 
                        ⫽ { GHC = [ "-Wall", "-fno-warn-name-shadowing" ] }
                    , exposed-modules =
                        [ "Distribution.Package.Dhall" ]
                    , hs-source-dirs =
                        [ "lib" ]
                    , other-extensions =
                        [ extensions.ApplicativeDo True
                        , extensions.GADTs True
                        , extensions.GeneralizedNewtypeDeriving True
                        , extensions.LambdaCase True
                        , extensions.OverloadedStrings True
                        , extensions.RecordWildCards True
                        , extensions.TypeApplications True
                        ]
                    , other-modules =
                        [ "Dhall.Extra" ]
                    }
              , guard =
                  always
              }
            ]
          ] : Optional
              (./dhall/types/Guarded.dhall  ./dhall/types/Library.dhall )
      , license =
          licenses.MIT {=}
      , name =
          "dhall-to-cabal"
      , version =
          [ +0, +1, +0 ]
      , test-suites =
          [ { name =
                "golden-tests"
            , test-suite =
                [ { body =
                        ./dhall/defaults/TestSuite.dhall 
                      ⫽ { build-depends =
                            [ common-deps.bytestring
                            , common-deps.base
                            , common-deps.Cabal
                            , common-deps.text
                            , package "tasty" (majorBoundVersion [ +0, +11 ])
                            , package "filepath" (majorBoundVersion [ +1, +4 ])
                            , common-deps.dhall-to-cabal
                            , package
                              "tasty-golden"
                              (majorBoundVersion [ +2, +3 ])
                            , package "Diff" (majorBoundVersion [ +0, +3, +4 ])
                            ]
                        , hs-source-dirs =
                            [ "golden-tests" ]
                        , type =
                            TestTypes.exitcode-stdio
                            { main-is = "GoldenTests.hs" }
                        }
                  , guard =
                      always
                  }
                ]
            }
          ]
      }
