import {URL} from 'url';

import {Syntax} from './options';

/**
 * This interface represents an [importer]. When the importer is invoked with a
 * string `string`:
 *
 * [importer]: ../spec/modules.md#importer
 *
 * - Let `fromImport` be `true` if the importer is being run for an `@import`
 *   and `false` otherwise.
 *
 * - Let `url` be the result of calling `findFileUrl` with `url` and
 *   `fromImport`.
 *
 * - If `url` is null, return null.
 *
 * - If `url`'s scheme is not `file`, throw an error.
 *
 * - Let `resolved` be the result of [resolving `url`].
 *
 * - If `resolved` is null, return null.
 *
 * - Let `text` be the contents of the file at `resolved`.
 *
 * - Let `syntax` be:
 *   - "scss" if `url` ends in `.scss`.
 *   - "indented" if `url` ends in `.sass`.
 *   - "css" if `url` ends in `.css`.
 *
 *   > The algorithm for resolving a `file:` URL guarantees that `url` will have
 *   > one of these extensions.
 *
 * - Return `text`, `syntax`, and `resolved`.
 *
 * [resolving `url`]: ../spec/modules.md#resolving-a-file-url
 */
interface SyncFileImporter {
  findFileUrl(
    url: string,
    options: {fromImport: boolean}
  ): FileImporterResult | null;

  canonicalize?: never;
}

/**
 * This interface represents an [importer]. It has the same behavior as
 * `SyncFileImporter`, except that if `findFileUrl` returns a `Promise` it waits
 * for it to complete and uses its value as the return value. If a `Promise`
 * emits an error, it rethrows that error.
 */
interface AsyncFileImporter {
  findFileUrl(
    url: string,
    options: {fromImport: boolean}
  ): Promise<FileImporterResult | null> | FileImporterResult | null;

  canonicalize?: never;
}

export interface FileImporterResult {
  /**
   * The partially-resolved `file:` URL of a file on disk.
   *
   * > Because this is [resolved] after being returned, it doesn't need to be a
   * > full canonical URL. Users' `FileImporter`s may simply append the relative
   * > URL to a path and let the compiler resolve extensions, partials, and
   * > index files.
   *
   * [resolved]: ../spec/modules.md#resolving-a-file-url
   */
  url: URL;

  /**
   * A browser-accessible URL indicating the resolved location of the imported
   * stylesheet.
   *
   * The implementation must use this URL in source maps to refer to source
   * spans in `css`.
   */
  sourceMapUrl?: URL;
}

/**
 * This interface represents an [importer]. When the importer is invoked with a
 * string `string`:
 *
 * [importer]: ../spec/modules.md#importer
 *
 * - Let `fromImport` be `true` if the importer is being run for an `@import`
 *   and `false` otherwise.
 *
 * - Let `url` be the result of calling `canonicalize` with `url` and
 *   `fromImport`.
 *
 * - If `url` is null, return null.
 *
 * - Let `result` be the result of calling `load` with `url`.
 *
 * - If `result` is null, return null.
 *
 * - Let `syntax` be `result.syntax`.
 *
 * - Throw an error if `syntax` is not "scss", "indented", or "css".
 *
 * - Otherwise, throw an error.
 *
 * - Return `result.css`, `syntax`, and `url`.
 *
 * [resolving `url`]: ../spec/modules.md#resolving-a-file-url
 */
interface SyncImporter {
  canonicalize(url: string, options: {fromImport: boolean}): URL | null;
  load(canonicalUrl: URL): ImporterResult | null;
  findFileUrl?: never;
}

/**
 * This interface represents an [importer]. It has the same behavior as
 * `SyncImporter`, except that if `canonicalize` or `load` return a `Promise` it
 * waits for it to complete and uses its value as the return value. If a
 * `Promise` emits an error, it rethrows that error.
 */
interface AsyncImporter {
  canonicalize(
    url: string,
    options: {fromImport: boolean}
  ): Promise<URL | null> | URL | null;

  load(
    canonicalUrl: URL
  ): Promise<ImporterResult | null> | ImporterResult | null;

  findFileUrl?: never;
}

export interface ImporterResult {
  /** The contents of stylesheet loaded by an importer. */
  css: string;

  /** The syntax to use to parse `css`. */
  syntax: Syntax;

  /**
   * A browser-accessible URL indicating the resolved location of the imported
   * stylesheet.
   *
   * The implementation must use this URL in source maps to refer to source
   * spans in `css`.
   */
  sourceMapUrl?: URL;
}

/**
 * > This type is exported so that TypeScript users can explicitly write a class
 * > that `implements FileImporter`.
 */
export type FileImporter<sync extends 'sync' | 'async'> = sync extends 'async'
  ? AsyncFileImporter
  : SyncFileImporter;

/**
 * > This type is exported so that TypeScript users can explicitly write a class
 * > that `implements Importer`.
 */
export type Importer<sync extends 'sync' | 'async'> = sync extends 'async'
  ? AsyncImporter
  : SyncImporter;
