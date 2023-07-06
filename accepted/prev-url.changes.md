## Draft 2.0

* Expand the proposal to cover the embedded protocol as well.

* Always pass `containingUrl` to `FilesystemImporter`s, since they always return
  `file:` canonical URLs and are never invoked for absolute `file:` URLs.

## Draft 1.1

* Throw an error when an importer returns a canonical URL using its
  non-canonical schemes.

## Draft 1

* Initial draft.
