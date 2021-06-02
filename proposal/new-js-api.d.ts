/**
 * # New JavaScript API
 */

import {RawSourceMap} from 'source-map-js'; // https://www.npmjs.com/package/source-map-js

export type Syntax = 'scss' | 'sass' | 'css';

export type OutputStyle = 'expanded' | 'compressed';

interface Options<sync extends 'sync' | 'async'> {
  alertAscii?: boolean;
  alertColor?: boolean;
  // TODO(awjin): functions?: Callable<sync>[];
  // TODO(awjin): importers?: Importer<sync>[];
  loadPaths?: string[];
  // TODO(awjin): logger?: Logger;
  quietDeps?: boolean;
  sourceMap?: boolean;
  style?: OutputStyle;
  verbose?: boolean;
}

type StringOptions<sync extends 'sync' | 'async'> = Options<sync> & {
  syntax?: Syntax;
} & (
    | {
        url?: string;
      }
    | {
        // TODO(awjin): importer: Importer<sync>;
        url: string;
      }
  );

export interface CompileResult {
  css: string;
  includedUrls: Set<string>;
  sourceMap?: RawSourceMap;
}

export interface SassException extends Error {
  // TODO(awjin): Mark this as `override` once TS 4.3 is released.
  message: string;

  sassMessage: string;

  sassStack?: string;

  // TODO(awjin): Mark this as `override` once TS 4.3 is released.
  stack?: string;

  // TODO(awjin): Mark this as `override` once TS 4.3 is released.
  toString(): string;
}

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
