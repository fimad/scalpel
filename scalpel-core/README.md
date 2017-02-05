Scalpel Core
============

Scalpel core provides a subset of the
[scalpel](https://hackage.haskell.org/package/scalpel) web scraping library that
is intended to have lightweight dependencies and to be free of all non-Haskell
dependencies.

Notably this package does not contain any networking support. Users who desire a
batteries include solution should depend on `scalpel` which does include
networking support instead of `scalpel-core`.

More thorough documentation including example code can be found in the
documentation of the [scalpel](https://hackage.haskell.org/package/scalpel)
package.
