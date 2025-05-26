# Change Log

## HEAD

- switch to more efficient `html-parser` (and `Data.Text` only instead of `StringLike str`)

## 0.6.2.2

- Fix build breakage on GHC 9.8 / mtl 2.3

## 0.6.2.1

- Match Content-Type case-insensitively.

## 0.6.2

- Add the monad transformer `ScraperT`.

## 0.6.1

- Support GHC 8.8.

## 0.6.0

### Breaking Changes

- `anySelector` now captures text nodes. This causes different results when used
  with a plural scraper (e.g. `chroots`). Usage with a singular scraper (e.g.
  `chroot`) should be unaffected.
- The dependency on `curl` has been replaced with `http-client` and
  `http-client-tls`. This has the following observable changes.
  - `scrapeURLWithOpts` is removed.
  - The `Config` type used with `scrapeURLWithConfig` no longer contains a list
    of curl options. Instead it now takes a `Maybe Manager` from `http-client`.
  - The `Decoder` function type now takes in a `Response` type from
    `http-client`.
  - `scrapeURL` will now throw an exception if there is a problem connecting to
     a URL.

### Other Changes

- Remove `Ord` constraint from public APIs.
- Add `atDepth` operator which allows for selecting nodes at a specified depth
  in relation to another node (#21).
- Fix issue selecting malformed HTML where `"a" // "c"` would not match
  `<a><b><c></c></a></b>`.
- Add `textSelector` for selecting text nodes.
- Add `SerialScraper` type and associated primitives (#48).

## 0.5.1

- Fix bug (#59, #54) in DFS traversal order.

## 0.5.0

- Split `scalpel` into two packages: `scalpel` and `scalpel-core`. The latter
  does not provide networking support and does not depend on curl.

## 0.4.1

- Added `notP` attribute predicate.

## 0.4.0

- Add the `chroot` tricks (#23 and #25) to README.md and added examples.
- Fix backtracking that occurs when using `guard` and `chroot`.
- Fix bug where the same tag may appear in the result set multiple times.
- Performance optimizations when using the (//) operator.
- Make Scraper an instance of MonadFail. Practically this means that failed
  pattern matches in `<-` expressions within a do block will evaluate to mzero
  instead of throwing an error and bringing down the entire script.
- Pluralized scrapers will now return the empty list instead mzero when there
  are no matches.
- Add the `position` scraper which provides the index of the current sub-tree
  within the context of a `chroots`'s do-block.

## 0.3.1

- Added the `innerHTML` and `innerHTMLs` scraper.
- Added the `match` function which allows for the creation of arbitrary
  attribute predicates.
- Fixed build breakage with GHC 8.0.1.

## 0.3.0.1

- Make tag and attribute matching case-insensitive.

## 0.3.0

- Added benchmarks and many optimizations.
- The `select` method is removed from the public API.
- Many methods now have a constraint that the string type parametrizing
  TagSoup's tag type now must be order-able.
- Added `scrapeUrlWithConfig` that will hopefully put an end to multiplying
  `scrapeUrlWith*` methods.
- The default behaviour of the `scrapeUrl*` methods is to attempt to infer the
  character encoding from the `Content-Type` header.

## 0.2.1.1

- Cleanup stale instance references in documentation of TagName and
  AttributeName.

## 0.2.1

- Made Scraper an instance of MonadPlus.

## 0.2.0.1

- Fixed examples in documentation and added an examples folder for ready to
  compile examples. Added travis tests to ensures that examples remain
  compilable.

## 0.2.0

- Removed the StringLike parameter from the Selector, Selectable,
  AttributePredicate, AttributeName, and TagName types. Instead they are now
  agnostic to the underlying string type, and are only constructable with
  Strings and the Any type.

## 0.1.3.1

- Tighten dependencies and drop download-curl all together.

## 0.1.3

- Add the html and html scraper primitives for extracting raw HTML.

## 0.1.2

- Make scrapeURL follow redirects by default.
- Expose a new function scrapeURLWithOpts that takes a list of curl options.
- Fix bug (#2) where image tags that do not have a trailing "/" are not
  selectable.

## 0.1.1

- Tighten dependencies on download-curl.

## 0.1.0

- First version!
