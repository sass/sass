## Draft 1.2

* Export `NodePackageImporter` type, and set `_NodePackageImporterBrand` to
  unknown.

## Draft 1.1

* Throw an error if `nodePackageImporter` is used in the browser or other
  environment without filesystem access.

* Remove specified order in the global import list, as users can specify the
  order within the `importers` option.

* Specify importer ordering for the Legacy API.

## Draft 1

* Initial draft
