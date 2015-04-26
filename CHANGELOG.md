# Change Log

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
