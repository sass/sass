## Draft 1.5

* Specify that Package Importers must not reject URL patterns that other Package
  Importers may be able to canonicalize.

* Specify that the Node Package Importer throws if `entryPointPath` is not
  passed and `require.main.filename` is not available.

## Draft 1.4

* Allow NodePackageImporter to accept an entry point path.

* Change Embedded protocol entry point to a path.

## Draft 1.3

* Handle empty subpath in "Resolving package exports" subprocedure.

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
