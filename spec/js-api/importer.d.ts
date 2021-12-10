import {Syntax} from './options';
import {PromiseOr} from './util/promise_or';

/**
 * This interface represents an [importer]. When the importer is invoked with a
 * string `string`:
 *
 * [importer]: ../spec/modules.md#importer
 *
 * - If `string` is an absolute URL whose scheme is `file`:
 *
 *   - Let `url` be string.
 *
 * - Otherwise:
 *
 *   - Let `fromImport` be `true` if the importer is being run for an `@import`
 *     and `false` otherwise.
 *
 *   - Let `url` be the result of calling `findFileUrl` with `string` and
 *     `fromImport`. If it returns a promise, wait for it to complete and use
 *     its value instead, or rethrow its error if it rejects.
 *
 *   - If `url` is null, return null.
 *
 *   - If `url`'s scheme is not `file`, throw an error.
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
export interface FileImporter<
  sync extends 'sync' | 'async' = 'sync' | 'async'
> {
  findFileUrl(
    url: string,
    options: {fromImport: boolean}
  ): PromiseOr<URL | null, sync>;

  canonicalize?: never;
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
 *   `fromImport`. If it returns a promise, wait for it to complete and use its
 *   value instead, or rethrow its error if it rejects.
 *
 * - If `url` is null, return null.
 *
 * - Let `result` be the result of calling `load` with `url`. If it returns a
 *   promise, wait for it to complete and use its value instead, or rethrow its
 *   error if it rejects.
 *
 * - If `result` is null, return null.
 *
 * - Let `syntax` be `result.syntax`.
 *
 * - Throw an error if `syntax` is not "scss", "indented", or "css".
 *
 * - Otherwise, throw an error.
 *
 * - Return `result.contents`, `syntax`, and `url`.
 *
 * [resolving `url`]: ../spec/modules.md#resolving-a-file-url
 */
export interface Importer<sync extends 'sync' | 'async' = 'sync' | 'async'> {
  canonicalize(
    url: string,
    options: {fromImport: boolean}
  ): PromiseOr<URL | null, sync>;

  load(canonicalUrl: URL): PromiseOr<ImporterResult | null, sync>;

  findFileUrl?: never;
}

export interface ImporterResult {
  /** The contents of stylesheet loaded by an importer. */
  contents: string;

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
