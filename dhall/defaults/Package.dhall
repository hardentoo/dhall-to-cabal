{ author =
    ""
, flags =
    [] : List ../types/Flag.dhall 
, benchmarks =
    [] : List
         { benchmark :
             ../types/Guarded.dhall  ../types/Benchmark.dhall 
         , name :
             Text
         }
, bug-reports =
    ""
, build-type =
    [ (constructors ../types/BuildType.dhall ).Simple {=}
    ] : Optional ../types/BuildType.dhall 
, cabal-version =
    ../types/Version/v.dhall  "2.0"
, category =
    ""
, copyright =
    ""
, data-dir =
    ""
, data-files =
    [] : List Text
, description =
    ""
, executables =
    [] : List
         { executable :
             ../types/Guarded.dhall  ../types/Executable.dhall 
         , name :
             Text
         }
, extra-doc-files =
    [] : List Text
, extra-source-files =
    [] : List Text
, extra-tmp-files =
    [] : List Text
, foreign-libraries =
    [] : List
         { foreign-lib :
             ../types/Guarded.dhall  ../types/ForeignLibrary.dhall 
         , name :
             Text
         }
, homepage =
    ""
, library =
    [] : Optional (../types/Guarded.dhall  ../types/Library.dhall )
, license =
    (constructors ../types/License.dhall ).Unspecified {=}
, license-files =
    [] : List Text
, maintainer =
    ""
, name =
    ""
, package-url =
    ""
, source-repos =
    [] : List ../types/SourceRepo.dhall 
, stability =
    ""
, sub-libraries =
    [] : List
         { library :
             ../types/Guarded.dhall  ../types/Library.dhall 
         , name :
             Text
         }
, synopsis =
    ""
, test-suites =
    [] : List
         { name :
             Text
         , test-suite :
             ../types/Guarded.dhall  ../types/TestSuite.dhall 
         }
, tested-with =
    [] : List
         { compiler :
             ../types/Compiler.dhall 
         , version :
             ../types/VersionRange.dhall 
         }
, x-fields =
    [] : List { _1 : Text, _2 : Text }
, custom-setup =
    [] : Optional ../types/SetupBuildInfo.dhall 
}
