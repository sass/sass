# Importer API

> Interfaces for user-declared importers that customize how Sass loads
> stylesheet dependencies.

```ts
import {Syntax} from './options';
import {PromiseOr} from './util/promise_or';
```

## Table of Contents

* [Types](#types)
  * [`CanonicalizeContext`](#canonicalizecontext)
  * [`FileImporter`](#fileimporter)
  * [`Importer`](#importer)
    * [`nonCanonicalScheme`](#noncanonicalscheme)
  * [`NodePackageImporter`](#nodepackageimporter)
  * [`ImporterResult`](#importerresult)

## Types

### `CanonicalizeContext`

This is a data object passed into calls to `Importer.canonicalize()` and
`FileImporter.findFileUrl()`. Its fields are set as part of the function
invocations.

```ts
export interface CanonicalizeContext {
  fromImport: boolean;
  containingUrl: URL | null;
}
```

### `FileImporter`

This interface represents an [importer]. When the importer is invoked with a
string `string`:

[importer]: ../modules.md#importer

* If `string` is an absolute URL whose scheme is `file`:

  * Let `url` be string.

* Otherwise:

  * Let `fromImport` be `true` if the importer is being run for an `@import` and
    `false` otherwise.

  * Let `containingUrl` be the canonical URL of the [current source file] if it
    has one, or undefined otherwise.

  * Let `url` be the result of calling `findFileUrl` with `string`, `fromImport`,
    and `containingUrl`. If it returns a promise, wait for it to complete and use
    its value instead, or rethrow its error if it rejects.

  * If `url` is null, return null.

  * If `url`'s scheme is not `file`, throw an error.

  [current source file]: ../spec.md#current-source-file

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
    context: CanonicalizeContext
  ): PromiseOr<URL | null, sync>;

  canonicalize?: never;
}
```

### `Importer`

This interface represents an [importer]. When the importer is invoked with a
string `string`:

* Let `fromImport` be `true` if the importer is being run for an `@import` and
  `false` otherwise.

* If `string` is a relative URL, or if it's an absolute URL whose scheme is
  non-canonical for this importer, let `containingUrl` be the canonical URL of
  the [current source file]. Otherwise, or if the current source file has no
  canonical URL, let `containingUrl` be undefined.

* Let `url` be the result of calling `canonicalize` with `string`, `fromImport`,
  and `containingUrl`. If it returns a promise, wait for it to complete and use
  its value instead, or rethrow its error if it rejects.

* If the scheme of `url` is [non-canonical] for this importer, throw an error.

  [non-canonical]: #noncanonicalscheme

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
    context: CanonicalizeContext
  ): PromiseOr<URL | null, sync>;

  load(canonicalUrl: URL): PromiseOr<ImporterResult | null, sync>;

  findFileUrl?: never;
```

#### `nonCanonicalScheme`

The set of URL schemes that are considered *non-canonical* for this importer. If
this is a single string, treat it as a list containing that string.

Before beginning compilation, throw an error if any element of this is empty or
contains a character other than a lowercase ASCII letter, an ASCII numeral,
U+002B (`+`), U+002D (`-`), or U+002E (`.`).

> Uppercase letters are normalized to lowercase in the `URL` constructor, so for
> simplicity and efficiency we only allow lowercase here.

```ts
nonCanonicalScheme?: string | string[];
```

```ts
} // Importer
```

### `NodePackageImporter`

```ts
export class NodePackageImporter {
  entryPointPath?: string;

  constructor(entryPointPath?: string);
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
