# Importer API

> Interfaces for user-declared importers that customize how Sass loads
> stylesheet dependencies.

```ts
import {Syntax} from './options';
import {PromiseOr} from './util/promise_or';
```

## Table of Contents

* [Types](#types)
  * [`FileImporter`](#fileimporter)
  * [`Importer`](#importer)
  * [`ImporterResult`](#importerresult)

## Types

### `FileImporter`

This interface represents an [importer]. When the importer is invoked with a
string `string`:

[importer]: ../modules.md#importer

* If `string` is an absolute URL whose scheme is `file`:

  * Let `url` be string.

* Otherwise:

  * Let `fromImport` be `true` if the importer is being run for an `@import` and
    `false` otherwise.

  * Let `url` be the result of calling `findFileUrl` with `string` and
    `fromImport`. If it returns a promise, wait for it to complete and use its
    value instead, or rethrow its error if it rejects.

  * If `url` is null, return null.

  * If `url`'s scheme is not `file`, throw an error.

* Let `resolved` be the result of [resolving `url`].

  [resolving `url`]: ../modules.md#resolving-a-file-url

* If `resolved` is null, return null.

* Let `text` be the contents of the file at `resolved`.

* Let `syntax` be:
  * "scss" if `url` ends in `.scss`.
  * "indented" if `url` ends in `.sass`.
  * "css" if `url` ends in `.css`.

  > The algorithm for resolving a `file:` URL guarantees that `url` will have
  > one of these extensions.

* Return `text`, `syntax`, and `resolved`.

```ts
export interface FileImporter<
  sync extends 'sync' | 'async' = 'sync' | 'async'
> {
  findFileUrl(
    url: string,
    options: {fromImport: boolean}
  ): PromiseOr<URL | null, sync>;

  canonicalize?: never;
}
```

### `Importer`

This interface represents an [importer]. When the importer is invoked with a
string `string`:

* Let `fromImport` be `true` if the importer is being run for an `@import` and
  `false` otherwise.

* Let `url` be the result of calling `canonicalize` with `url` and `fromImport`.
  If it returns a promise, wait for it to complete and use its value instead, or
  rethrow its error if it rejects.

* If `url` is null, return null.

* Let `result` be the result of calling `load` with `url`. If it returns a
  promise, wait for it to complete and use its value instead, or rethrow its
  error if it rejects.

* If `result` is null, return null.

* Throw an error if `result.syntax` is not "scss", "indented", or "css".

* If `result.sourceMapUrl` is defined and the implementation generates a source
  map, the implementation must use this URL in the source map to refer to source
  spans in `result.contents`.

* Return `result.contents`, `result.syntax`, and `url`.

```ts
export interface Importer<sync extends 'sync' | 'async' = 'sync' | 'async'> {
  canonicalize(
    url: string,
    options: {fromImport: boolean}
  ): PromiseOr<URL | null, sync>;

  load(canonicalUrl: URL): PromiseOr<ImporterResult | null, sync>;

  findFileUrl?: never;
}
```

### `ImporterResult`

```ts
export interface ImporterResult {
  contents: string;

  syntax: Syntax;

  sourceMapUrl?: URL;
}
```
