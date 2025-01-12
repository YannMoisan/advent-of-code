# PB1

sbt.librarymanagement.ResolveException: Error downloading org.scalatest:scalatest_3:3.2.3

# PB2

```
[error] Modules were resolved with conflicting cross-version suffixes in ProjectRef(uri("file:/Users/yamo/projects/perso/advent-of-code/"), "advent-of-code-2024"):
[error]    org.scalatest:scalatest-featurespec _3, _2.13
[error]    org.scalatest:scalatest-shouldmatchers _3, _2.13
[error]    org.scalatest:scalatest-matchers-core _3, _2.13
[error]    org.scalatest:scalatest-diagrams _3, _2.13
[error]    org.scalatest:scalatest _3, _2.13
[error]    org.scalatest:scalatest-refspec _3, _2.13
[error]    org.scalatest:scalatest-funspec _3, _2.13
[error]    org.scalatest:scalatest-freespec _3, _2.13
[error]    org.scala-lang.modules:scala-xml _3, _2.13
[error]    org.scalatest:scalatest-propspec _3, _2.13
[error]    org.scalactic:scalactic _3, _2.13
[error]    org.scalatest:scalatest-flatspec _3, _2.13
[error]    org.scalatest:scalatest-funsuite _3, _2.13
[error]    org.scalatest:scalatest-wordspec _3, _2.13
[error]    org.scalatest:scalatest-mustmatchers _3, _2.13
[error]    org.scalatest:scalatest-core _3, _2.13
```

=> on fait un module core3

Pb3

```
[error] -- Error: /Users/yamo/projects/perso/advent-of-code/core3/src/main/scala/com/yannmoisan/util/graph/Graph.scala:71:19
[error] 71 |      val h :: t = stack
[error]    |                   ^^^^^
[error]    |pattern's type ::[Int] is more specialized than the right hand side expression's type List[Int]
[error]    |
[error]    |If the narrowing is intentional, this can be communicated by adding `: @unchecked` after the expression,
[error]    |which may result in a MatchError at runtime.
[error]    |This patch can be rewritten automatically under -rewrite -source 3.2-migration.
```

Pb4

[info]   ExceptionInInitializerError was thrown during property evaluation. (AllPuzzlesSpec.scala:16)
[info]     Message: "None"

Pb5 App args


Caused by: java.lang.NullPointerException: Cannot read the array length because the return value of "Puzzles$.args()" is null
	at Puzzles$.<clinit>(Puzzles.scala:49)



sbt:advent-of-code> migrateDependencies advent-of-code-2024
[info]
[info] Starting migration of libraries and compiler plugins in project 'advent-of-code-2024'
[warn]
[warn] Versions to update:
[warn] "org.scalatest" %% "scalatest" % "3.2.9" % Test (Other versions: 3.2.10, ..., 3.3.0-SNAP4)
[info]

sbt:advent-of-code> migrateScalacOptions advent-of-code-2024
[info]
[info] Starting migration of scalacOptions in project 'advent-of-code-2024'
[info]
[info] Valid scalacOptions:
[info] -encoding utf8
[info] -feature
[info] -language:existentials
[info] -language:experimental.macros
[info] -language:higherKinds
[info] -language:implicitConversions
[info] -unchecked
[info] -Xfatal-warnings
[info] -Wunused:nowarn
[info] -Wunused:implicits
[info] -Wunused:explicits
[info] -Wunused:imports
[info] -Wunused:locals
[info] -Wunused:params
[info] -Wunused:patvars
[info] -Wunused:privates
[info] -Wvalue-discard
[warn]
[warn] Renamed scalacOptions:
[warn] -explaintypes      -> -explain
[warn] -Xcheckinit        -> -Ysafe-init
[warn] -Xlint:deprecation -> -deprecation
[warn]
[warn] Removed scalacOptions:
[warn] -Xlint:adapted-args
[warn] -Xlint:constant
[warn] -Xlint:delayedinit-select
[warn] -Xlint:doc-detached
[warn] -Xlint:inaccessible
[warn] -Xlint:infer-any
[warn] -Xlint:missing-interpolator
[warn] -Xlint:nullary-unit
[warn] -Xlint:option-implicit
[warn] -Xlint:package-object-classes
[warn] -Xlint:poly-implicit-overload
[warn] -Xlint:private-shadow
[warn] -Xlint:stars-align
[warn] -Xlint:type-parameter-shadow
[warn] -Wdead-code
[warn] -Wextra-implicit
[warn] -Wnumeric-widen
[warn] -Yrangepos
[info]
[info] For a full list of options check the Compiler Options Lookup Table at
[info] https://docs.scala-lang.org/scala3/guides/migration/options-lookup.html
[success] Total time: 8 s, completed Dec 2, 2024, 11:10:29 PM
