import {URL} from 'url';
import {RawSourceMap} from 'source-map-js';

import {Options, StringOptions} from './options';

export interface CompileResult {
  css: string;
  loadedUrls: URL[];
  sourceMap?: RawSourceMap;
}

/** @hidden */
export function compile(path: string, options?: Options<'sync'>): CompileResult;

/** @hidden */
export function compileAsync(
  path: string,
  options?: Options<'async'>
): Promise<CompileResult>;

/** @hidden */
export function compileString(
  source: string,
  options?: StringOptions<'sync'>
): CompileResult;

/** @hidden */
export function compileStringAsync(
  source: string,
  options?: StringOptions<'async'>
): Promise<CompileResult>;
