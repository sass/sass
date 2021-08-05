/**
 * # New JS Importer: Draft 1.1
 *
 * Issue: https://github.com/sass/sass/issues/2509
 * Changelog: <new-js-importer.changes.md>
 *
 * Note: this issue builds on the New JavaScript API proposal.
 *
 * ## Background
 *
 * > This section is non-normative.
 *
 * Sass's current JavaScript API was inherited from Node Sass, which developed
 * over time in an _ad hoc_ manner. The importer API in particular has a number
 * of notable issues:
 *
 * - It doesn't respect URL semantics. In particular, it doesn't provide any
 *   guarantee that `@import "./bar"` within a stylesheet with URL `foo` means
 *   the same thing as `@import "foo/bar"`, even when resolved by the same
 *   importer.
 *
 * - It doesn't have a notion of resolving an imported URL to an absolute
 *   URL—for example, a Webpack importer can't resolve `~foo` to
 *   `file:///home/nex3/app/node_modules/foo/_index.scss`. This makes
 *   canonicalization of imported stylesheets impossible, and means that
 *   importers are fundamentally less expressive than built-in filesystem
 *   imports. It also means that the URLs reported in source maps and other APIs
 *   are unlikely to be resolvable.
 *
 * - It doesn't support the indented syntax for stylesheets that aren't on disk.
 *
 * - Its notion of "redirecting" to a file import has some strange corner
 *   cases—for example, a relative path redirect can be resolved relative to the
 *   file that contains the import, relative to the working directory, _or_
 *   relative to an import path.
 *
 * - It has limited error checking for invalid responses.
 *
 * - It uses the string `"stdin"` to refer to files passed by data, even when
 *   they don't come from standard input. This could potentially conflict with a
 *   real file named `"stdin"`.
 *
 * - The `this` context includes a bunch of undocumented information without a
 *   clear use-case.
 *
 * Given all of this, the API needs an overhaul. Dart Sass's Dart API has [an
 * `Importer` API] (itself based on Ruby Sass's API) that can serve as a useful
 * model.
 *
 * [an `importer` api]: https://pub.dev/documentation/sass/latest/sass/Importer-class.html
 *
 * ## Summary
 *
 * > This section is non-normative.
 *
 * Rather than modeling importers as single-function callbacks, we'll model them
 * as objects that expose multiple methods. The most important of these methods
 * are those that _canonicalize_ a URL and _load_ a canonical URL.
 *
 * ### Canonicalizing
 *
 * The first step determines the canonical URL for a stylesheet. Each stylesheet
 * has exactly one canonical URL that in turn refers to exactly one stylesheet.
 * The canonical URL must be absolute, including a scheme, but the specific
 * structure is up to the importer. In most cases, the stylesheet in question
 * will exist on disk and the importer will just return a `file:` URL for it.
 *
 * The `canonicalize()` method takes a URL string that may be either relative or
 * absolute. If the importer recognizes that URL, it returns a corresponding
 * absolute URL (including a scheme). This is the _canonical URL_ for the
 * stylesheet in question. Although the input URL may omit a file extension or
 * an initial underscore, the canonical URL must be fully resolved.
 *
 * For a stylesheet that's loaded from the filesystem, the canonical URL will be
 * the absolute `file:` URL of the physical file on disk. If it's generated
 * in-memory, the importer should choose a custom URL scheme to guarantee that
 * its canonical URLs don't conflict with any other importer's.
 *
 * For example, if you're loading Sass files from a database, you might use the
 * scheme `db:`. The canonical URL for a stylesheet associated with key `styles`
 * in the database might be `db:styles`.
 *
 * Having a canonical URL for each stylesheet allows Sass to ensure that the
 * same stylesheet isn't loaded multiple times in the new module system.
 *
 * #### Canonicalizing Relative Loads
 *
 * When a stylesheet tries to load a relative URL, such as `@use "variables"`,
 * it's not clear from the document itself whether that refers to a file that
 * exists relative to the stylesheet or to another importer or load path. Here's
 * how the importer API resolves that ambiguity:
 *
 * * First, the relative URL is resolved relative to the canonical URL of the
 *   stylesheet that contained the `@use` (or `@forward` or `@import`). For
 *   example, if the canonical URL is `file:///path/to/my/_styles.scss`, then
 *   the resolved URL will be `file:///path/to/my/variables`.
 *
 * * This URL is then passed to the `canonicalize()` method of the importer that
 *   loaded the old stylesheet. (That means it's important for your importers to
 *   support absolute URLs!) If the importer recognizes it, it returns the
 *   canonical value which is then passed to that importer's `load()`;
 *   otherwise, it returns `null`.
 *
 * * If the old stylesheet's importer didn't recognize the URL, it's passed to
 *   all the `importers`' canonicalize functions in the order they appear in
 *   `options`, then checked for in all the `loadPaths`. If none of those
 *   recognizes it, the load fails.
 *
 * It's important that local relative paths take precedence over other importers
 * or load paths, because otherwise your local stylesheets could get
 * unexpectedly broken by a dependency adding a file with a conflicting name.
 *
 * ### Loading
 *
 * The second step actually loads the text of the stylesheet. The `load()`
 * method takes a canonical URL that was returned by `canonicalize()` and
 * returns the contents of the stylesheet at that URL. This is only called once
 * per compilation for each canonical URL; future loads of the same URL will
 * re-use either the existing module (for `@use` and `@forward`) or the parse
 * tree (for `@import`).
 *
 * ### `FileImporter`
 *
 * This proposal also adds a special type of importer known as a `FileImporter`.
 * This importer makes the common case of redirecting loads to somewhere on the
 * physical filesystem easier. It doesn't require the caller to implement
 * `load()`, since that's always going to be the same for files on disk.
 *
 * A `FileImporter` only needs to have one method: `findFileUrl()`, which takes
 * a relative URL and returns the absolute `file:` URL it should refer to.
 * However, this URL doesn't need to be fully canonicalized: the Sass compiler
 * will take care of resolving partials, file extensions, index files, and so
 * on.
 */

/** ## API */

import {URL} from 'url';

import {Options, Syntax} from './new-js-api';
import './new-js-api';

declare module './new-js-api' {
  interface Options<sync extends 'sync' | 'async'> {
    importers?: _Importer<sync>[];
  }
}

/** This definition supersedes the one in the New JavaScript API proposal. */
type StringOptions<sync extends 'sync' | 'async'> = Options<sync> & {
  syntax?: Syntax;
} & (
    | {url?: URL}
    | {
        importer: _Importer<sync>;
        url: URL;
      }
  );

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

/**
 * > This type allows options to refer to tersely refer to everything that
 * > represents an importer.
 */
type _Importer<sync extends 'sync' | 'async'> =
  | Importer<sync>
  | FileImporter<sync>;

/**
 * ### `compile()`
 *
 * This proposal modifies the `compile()` function's specification by also
 * passing `options.importers` to the [compiling a path] procedure as
 * `importers`.
 *
 * [compiling a path]: ../spec/spec.md#compiling-a-path
 *
 * In addition, when `compile()` is invoked, it should throw an error if any
 * objects in `options.importers` have both `findFileUrl` and `canonicalize`
 * fields.
 *
 * ### `compileString()`
 *
 * This proposal modifies the `compileString()` function's specification by also
 * passing `options.importers` to the [compiling a path] procedure as
 * `importers` and `options.impoter` as `importer`.
 *
 * [compiling a path]: ../spec/spec.md#compiling-a-path
 *
 * In addition, when `compileString()` is invoked, it should throw an error if
 * `options.importer` or any objects in `options.importers` have both
 * `findFileUrl` and `canonicalize` fields.
 */
