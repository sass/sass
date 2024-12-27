# Legacy Importer API

```ts
import {LegacyPluginThis} from './plugin_this';
```

## Table of Contents
## i am here

* [Types](#types)
  * [`LegacyImporterThis`](#legacyimporterthis)
    * [`fromImport`](#fromimport)
  * [`LegacyImporterResult`](#legacyimporterresult)
  * [`LegacySyncImporter`](#legacysyncimporter)
  * [`LegacyAsyncImporter`](#legacyasyncimporter)
  * [`LegacyImporter`](#legacyimporter)

## Types

### `LegacyImporterThis`

The interface for the `this` keyword for custom importers. The implementation
must invoke importers with an appropriate `this`.

```ts
interface LegacyImporterThis extends LegacyPluginThis {
```

#### `fromImport`

The implementation must set this field to true if this importer invocation was
caused by an `@import` statement and `false` otherwise.

> This allows importers to look for `.import.scss` stylesheets if and only if an
> `@import` is being resolved.

```ts
fromImport: boolean;
```

```ts
} // LegacyImporterThis
```

### `LegacyImporterResult`

```ts
export type LegacyImporterResult =
  | {file: string}
  | {contents: string}
  | Error
  | null;
```

### `LegacySyncImporter`

```ts
type LegacySyncImporter = (
  this: LegacyImporterThis,
  url: string,
  prev: string
) => LegacyImporterResult;
```

### `LegacyAsyncImporter`

```ts
type LegacyAsyncImporter = (
  this: LegacyImporterThis,
  url: string,
  prev: string,
  done: (result: LegacyImporterResult) => void
) => void;
```

### `LegacyImporter`

```ts
export type LegacyImporter<sync = 'sync' | 'async'> = sync extends 'async'
  ? LegacySyncImporter | LegacyAsyncImporter
  : LegacySyncImporter;
```
