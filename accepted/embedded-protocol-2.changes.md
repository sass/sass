## Draft 1.1

* No longer mark `CanonicalizeResponse.result`, `ImportResponse.result`, or
  `FileImportResponse.result` as optional at the language level since explicitly
  optional `oneof`s aren't supported.

* Document design decisions for cross-compilation state and outbound request
  IDs.

## Draft 1

* Initial draft.
