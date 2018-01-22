{ author             = ""
, benchmarks         = {=}
, bug-reports        = ""
, build-type         =
    [ < Simple = {=} | Configure : {} | Custom : {} | Make : {} >
    ] : Optional < Configure : {} | Custom : {} | Make : {} | Simple : {} >
, cabal-version      = [ +2, +0 ]
, category           = ""
, copyright          = ""
, data-directory     = ""
, data-files         = [] : List Text
, description        = ""
, executables        = {=}
, extra-doc-files    = [] : List Text
, extra-source-files = [] : List Text
, extra-temp-files   = [] : List Text
, foreign-libraries  = {=}
, homepage           = ""
, license            =
      < Unspecified       = {=}
      | AGPL              : Optional (List Natural)
      | AllRightsReserved : {}
      | Apache            : Optional (List Natural)
      | BSD2              : {}
      | BSD3              : {}
      | BSD4              : {}
      | GPL               : Optional (List Natural)
      | ISC               : {}
      | LGPL              : Optional (List Natural)
      | MIT               : {}
      | MPL               : List Natural
      | Other             : {}
      | PublicDomain      : {}
      >
    : < AGPL              : Optional (List Natural)
      | AllRightsReserved : {}
      | Apache            : Optional (List Natural)
      | BSD2              : {}
      | BSD3              : {}
      | BSD4              : {}
      | GPL               : Optional (List Natural)
      | ISC               : {}
      | LGPL              : Optional (List Natural)
      | MIT               : {}
      | MPL               : List Natural
      | Other             : {}
      | PublicDomain      : {}
      | Unspecified       : {}
      >
, license-files      = [] : List Text
, maintainer         = ""
, package            = { name = "", version = [] : List Natural }
, package-url        = ""
, source-repos       =
    [] : List { location : Optional Text, type : Optional < Git : {} > }
, stability          = ""
, sub-libraries      = {=}
, synopsis           = ""
, tested-with        =
    [] : List
         { compiler :
             < GHC    : {}
             | GHCJS  : {}
             | HBC    : {}
             | Helium : {}
             | Hugs   : {}
             | JHC    : {}
             | LHC    : {}
             | NHC    : {}
             | UHC    : {}
             | YHC    : {}
             >
         , version  : VersionRange
         }
, tests              = {=}
, x-fields           = [] : List { _1 : Text, _2 : Text }
}
