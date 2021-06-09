/**
 * # New JavaScript API: Draft 1
 *
 * *([Issue](https://github.com/sass/sass/issues/3056))*
 *
 * ## Background
 *
 * > This section is non-normative.
 *
 * Sass's [current JS API] grew organically from the Sass community's need to
 * interact deeply and programmatically with Sass compilations. Since it mostly
 * served as a wrapper around [LibSass], its quirks make it difficult for both
 * the reference and new implementations to support it.
 *
 * [current JS API]: https://github.com/sass/node-sass
 * [LibSass]: https://github.com/sass/libsass
 *
 * LibSass was deprecated recently, and the upcoming [Embedded Host] needs to
 * implement a JS API. Rather than retrofit the old API, it makes sense to
 * design a strict, well-specified new API that can be shared by the Embedded
 * Host, the reference implementation, and any future Sass implementations.
 *
 * [Embedded Host]: https://github.com/sass/embedded-host-node
 *
 * A new API optimized for consistency and reusability makes it easier for users
 * to move between Sass implementations with minimal breakages to their code. It
 * makes Sass a more robust infrastructure for build systems. It allows us to
 * support the Sass community as it continues to mature into best practices and
 * larger problem spaces.
 *
 * ## Summary
 *
 * > This section is non-normative.
 *
 * This proposal specifies synchronous and asynchronous functions for compiling
 * Sass input by path or by string. It also specifies all of the options for
 * configuring Sass compilations.
 *
 * This proposal is strongly influenced by the [reference implementation's API],
 * which is carefully designed, considered, and maintained by the core Sass
 * team, as well as the [Embedded Protocol], which was designed for reusability
 * across multiple compiler implementations.
 *
 * [reference implementation's API]: https://pub.dev/documentation/sass/latest/sass/sass-library.html
 * [Embedded Protocol]: https://github.com/sass/embedded-protocol
 */

/** ## API */

import {URL} from 'url';
import {RawSourceMap} from 'source-map-js'; // https://www.npmjs.com/package/source-map-js

/** The types of input syntax that the compiler can parse. */
type Syntax = 'scss' | 'sass' | 'css';

/**
 * The ways in which the compiler can format the emitted CSS.
 *
 * > The specifics of each format can vary from implementation to
 * > implementation. If an implementation wants to add a new OutputStyle, they
 * > should expand this type.
 */
type OutputStyle = 'expanded' | 'compressed';

/**
 * All of the options for a Sass compilation that are shared by compiling from a
 * path and by compiling from a string.
 */
interface Options<sync extends 'sync' | 'async'> {
  /**
   * If true, the compiler must use only ASCII characters in the formatted
   * message of errors and logs that aren't handled by a `logger`.
   *
   * @default false
   */
  alertAscii?: boolean;

  /**
   * If true, the compiler may use terminal colors in the formatted message of
   * errors and logs that aren't handled by a `logger`.
   *
   * > The specific format can vary from implementation to implementation.
   * > Compilers are not obligated to use terminal colors if they are irrelevant
   * > in the output environment.
   *
   * @default false
   */
  alertColor?: boolean;

  // TODO(awjin): functions?: Callable<sync>[];

  // TODO(awjin): importers?: Importer<sync>[];

  /** If set, the compiler must use these paths to resolve imports. */
  loadPaths?: string[];

  // TODO(awjin): logger?: Logger;

  /**
   * If true, the compiler must not print deprecation warnings for stylesheets
   * that are transitively loaded through an import path.
   *
   * @default false
   */
  quietDeps?: boolean;

  /**
   * If true, the compiler must set the sourceMap field of the `CompileResult`
   * to a sourceMap object.
   *
   * @default false
   */
  sourceMap?: boolean;

  /**
   * If present, the compiler must format the emitted CSS in this style.
   *
   * Implementations may support any amount of options, provided that:
   * - They support the 'expanded' option.
   * - They produce CSS that is semantically equivalent regardless of style.
   * - They throw an error if they receive a value for this option that they
   *   do not support.
   */
  style?: OutputStyle;

  /**
   * If `true`, the compiler must print every single deprecation warning it
   * encounters.
   *
   * If `false`, the compiler may choose not to print repeated deprecation
   * warnings.
   *
   * @default false
   */
  verbose?: boolean;
}

/** Additional options specific to compiling from a string. */
type StringOptions<sync extends 'sync' | 'async'> = Options<sync> & {
  /**
   * The compiler must parse `source` using this syntax.
   *
   * @default 'scss'
   */
  syntax?: Syntax;
} & (
    | {
        /** The compiler must treat this as the canonical URL of `source`. */
        url?: URL;
      }
    | {
        // TODO(awjin): importer: Importer<sync>;
        url: URL;
      }
  );

/** The error thrown by the compiler when a Sass compilation fails. */
export interface Exception extends Error {
  /**
   * The compiler supplies this error message to the JS runtime. This should
   * contain the description of the Sass exception as well as the Sass span and
   * stack (if available).
   *
   * This message must be passed directly to the super constructor.
   *
   * > The format can vary from implementation to implementation.
   */
  message: string;

  /**
   * The Sass error message, excluding the span and stack.
   *
   * > The format can vary from implementation to implementation.
   */
  sassMessage: string;

  /**
   * The Sass stack trace at the point the error was thrown.
   *
   * > The format can vary from implementation to implementation.
   */
  sassStack?: string;

  /**
   * Provides a formatted string with useful information about the error.
   *
   * > This likely includes the Sass error message, span, and stack. The format
   * > can vary from implementation to implementation.
   *
   * TODO(awjin): Mark this as `override` once TS 4.3 is released.
   */
  toString(): string;
}

/** The object returned by the compiler when a Sass compilation succeeds. */
export interface CompileResult {
  css: string;
  includedUrls: URL[];
  sourceMap?: RawSourceMap;
}

/**
 * [Compiles](../spec/spec.md#compiling-a-path) the Sass file at `path`.
 *
 * The compilation must respect the configuration specified by the `options`
 * object.
 *
 * If the compilation succeeds, the compiler returns a `CompileResult` object
 * composed as follows:
 * - `CompileResult.css` is set to the emitted CSS.
 * - `CompileResult.includedFiles` is set to a list of unique URLs included in
 *   the compilation. The order of URLs is not guaranteed.
 * - If `options.sourceMap` is `true`, `CompileResult.sourceMap` is set to a
 *   sourceMap object describing how sections of the Sass input correspond to
 *   sections of the CSS output.
 *
 *   > The structure of the sourceMap can vary from implementation to
 *   > implementation.
 *
 * If the compilation fails, the compiler throws an `Exception`.
 */
export function compile(path: string, options?: Options<'sync'>): CompileResult;

/**
 * Like `compile`, but runs asynchronously.
 *
 * Running asynchronously allows passing `AsyncCallable`s and `AsyncImporter`s
 * to the compiler rather than their synchronous counterparts.
 */
export function compileAsync(
  path: string,
  options?: Options<'async'>
): Promise<CompileResult>;

/**
 * [Compiles](../spec/spec.md#compiling-a-string) the Sass `source`.
 *
 * The compilation must respect the configuration specified by the `options`
 * object.
 *
 * If the compilation succeeds, the compiler returns a `CompileResult` object
 * composed as follows:
 * - `CompileResult.css` is set to the emitted CSS.
 * - `CompileResult.includedFiles` is set to a list of unique URLs included in
 *   the compilation. The order of URLs is not guaranteed.
 *   - If `options.url` is set, the url is included in the list.
 *   - Otherwise, `source` is excluded.
 * - If `options.sourceMap` is `true`, `CompileResult.sourceMap` is set to a
 *   sourceMap object describing how sections of the Sass input correspond to
 *   sections of the CSS output.
 *
 *   > The structure of the sourceMap can vary from implementation to
 *   > implementation.
 *
 * If the compilation fails, the compiler throws an `Exception`.
 */
export function compileString(
  source: string,
  options?: StringOptions<'sync'>
): CompileResult;

/**
 * Like `compileString`, but runs asynchronously.
 *
 * Running asynchronously allows passing `AsyncCallable`s and `AsyncImporter`s
 * to the compiler rather than their synchronous counterparts.
 */
export function compileStringAsync(
  source: string,
  options?: StringOptions<'async'>
): Promise<CompileResult>;
