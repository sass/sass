/**
 * # New JavaScript API
 */

// TODO(awjin): Add this as a dev-dependency.
import {RawSourceMap} from 'source-map-js';  // https://www.npmjs.com/package/source-map-js

export type Syntax = 'scss' | 'sass' | 'css';

export type OutputStyle = 'expanded' | 'compressed';

type Execution = 'sync' | 'async';

interface Options<executionType extends Execution> {
  // TODO(awjin): functions?: Callable<executionType>[];
  // TODO(awjin): importers?: Importer<executionType>[];
  loadPaths?: string[];
  sourceMap?: boolean;
  // TODO(awjin): logger?: Logger;
  style?: OutputStyle;
  charset?: boolean;
  color?: boolean;
  quietDeps?: boolean;
  verbose?: boolean;
}

export interface CompileResult {
  css: string;
  includedUrls: Set<string>;
  sourceMap?: RawSourceMap;
}

export interface SassException extends Error {
  /** Contains the error message and the Sass span (if available). */
  message: string;

  /** Contains the Sass and JS stack traces (if available). */
  stack?: string;

  /**
   * Prints a usefully formatted error. Includes the error message, the Sass
   * span (if available) and the Sass and JS stack traces (if available).
   */
  toString(): string;
}

type StringOptions<executionType extends Execution> = Options<executionType> & {
  syntax?: Syntax;
} & (
    | {
        url?: string;
      }
    | {
        url: string;
        // TODO(awjin): importer: Importer<executionType>;
      }
  );

export function compile(path: string, options?: Options<'sync'>): CompileResult;

export function compileAsync(
  path: string,
  options?: Options<'async'>
): Promise<CompileResult>;

export function compileString(
  source: string,
  options?: StringOptions<'sync'>
): CompileResult;

export function compileStringAsync(
  source: string,
  options?: StringOptions<'async'>
): Promise<CompileResult>;
