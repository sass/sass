import {URL} from 'url';
import {RawSourceMap} from 'source-map-js';

import {Options, StringOptions} from './options';

export interface CompileResult {
  css: string;
  loadedUrls: URL[];
  sourceMap?: RawSourceMap;
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
