/**
 * # New JavaScript API: Draft 1
 *
 * *([Issue](https://github.com/sass/sass/issues/3056))*
 */

/** ## API */

import {RawSourceMap} from 'source-map-js'; // https://www.npmjs.com/package/source-map-js

/** The types of input syntax that the compiler can parse. */
export type Syntax = 'scss' | 'sass' | 'css';

/**
 * The ways in which the compiler can format the emitted CSS.
 *
 * > The specifics of each format can vary from implementation to
 * > implementation.
 */
export type OutputStyle = 'expanded' | 'compressed';

/**
 * All of the options for a Sass compilation that are shared by compiling from a
 * path and by compiling from a string.
 *
 * These options take a conditional type `sync` that describes the compilation's
 * expected execution. E.g. for a synchronous compilation, the compiler should
 * require the options to be synchronous, with appropriately synchronous
 * members.
 */
interface Options<sync extends 'sync' | 'async'> {
  /**
   * If true, the compiler must use only ASCII characters in the formatted
   * message of errors and logs that aren't handled by a `logger` callback.
   *
   * @default false
   */
  alertAscii?: boolean;

  /**
   * If true, the compiler may use terminal colors in the formatted message of
   * errors and logs that aren't handled by a `logger` callback.
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

  /**
   * Paths for the compiler to use to resolve imports.
   *
   * This is equivalent to appending filesystem importers to `importers`.
   */
  loadPaths?: string[];

  // TODO(awjin): logger?: Logger;

  /**
   * If true, the compiler must not print deprecation warnings that come from
   * dependencies.
   *
   * A dependency is defined as any file that is not part of the main package.
   * This includes those loaded through `loadPaths` and may include those loaded
   * by custom `importers`.
   *
   * @default false
   */
  quietDeps?: boolean;

  /**
   * If true, the compiler must emit a sourceMap of type `RawSourceMap` along
   * with the compiled CSS.
   *
   * @default false
   */
  sourceMap?: boolean;

  /**
   * If present, the compiler must format the emitted CSS in this style.
   *
   * @default 'expanded'
   */
  style?: OutputStyle;

  /**
   * If present, the compiler must print all warnings without truncation.
   *
   * The reference implementation has a default cutoff of 5 prints per type of
   * warning.
   *
   * @default false
   */
  verbose?: boolean;
}

/** Additional options specific to compiling from a string. */
type StringOptions<sync extends 'sync' | 'async'> = Options<sync> & {
  /**
   * The syntax that the compiler should use to parse the string.
   *
   * @default 'scss'
   */
  syntax?: Syntax;
} & (
    | {
        /**
         * The location from which the string was loaded. If passed, the
         * compiler must resolve imports relative to `url`.
         */
        url?: string;
      }
    | {
        // TODO(awjin): importer: Importer<sync>;
        url: string;
      }
  );

/**
 * The object returned by a Sass compilation.
 */
export interface CompileResult {
  /** The compiled css. */
  css: string;

  /**
   * All URLs included in the compilation.
   *
   * If the compilation source was a string:
   * * If the root `url` was specified, `url` is included in the set.
   * * Otherwise, the source is excluded.
   */
  includedUrls: Set<string>;

  /**
   * A sourceMap that describes how sections in the Sass input correspond to
   * sections in the resulting CSS.
   *
   * See: https://www.npmjs.com/package/source-map-js
   */
  sourceMap?: RawSourceMap;
}

/**
 * The error thrown by the compiler when a Sass compilation fails.
 */
export interface SassException extends Error {
  /**
   * The error message provided to the JS runtime by the compiler. This should
   * contain the top-level description of the Sass exception as well as the Sass
   * span and stack (if available).
   *
   * > The format can vary from implementation to implementation.
   */
  message: string; // TODO(awjin): Mark this as `override` once TS 4.3 is released.

  /**
   * The top-level description of the Sass exception.
   *
   * Does not contain the Sass span or stack.
   */
  sassMessage: string;

  /**
   * The Sass stack trace at the point the error was thrown.
   *
   * > The format can vary from implementation to implementation.
   */
  sassStack?: string;

  /**
   * A nicely formatted string that contains useful information about the error.
   *
   * > This likely includes the Sass error message, span, and stack. The format
   * > can vary from implementation to implementation.
   */
  toString(): string; // TODO(awjin): Mark this as `override` once TS 4.3 is released.
}

/**
 * Compiles the Sass file at `path`.
 *
 * Throws a SassException if the compilation fails.
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
 * Compiles the Sass `source`.
 *
 * Throws a SassException if the compilation fails.
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
