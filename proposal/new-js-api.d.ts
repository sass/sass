/**
 * # New JavaScript API
 */

import {RawSourceMap} from 'source-map';  // https://github.com/mozilla/source-map

export function compile(path: string, options?: CompileOptions): CompileResult;

export function compileAsync(
  path: string,
  options?: CompileAsyncOptions
): Promise<CompileResult>;

export function compileString(
  source: string,
  options?: CompileStringOptions
): CompileResult;

export function compileStringAsync(
  source: string,
  options?: CompileStringAsyncOptions
): Promise<CompileResult>;

export interface CompileResult {
  css: string;
  sourceMap?: RawSourceMap;
  includedUrls?: string[];
}

export type CompileOptions = SharedOptions & Plugins;

export type CompileAsyncOptions = SharedOptions & AsyncPlugins;

export type CompileStringOptions = (StringOptions | StringWithImporterOptions) &
  Plugins;

export type CompileStringAsyncOptions = (
  | StringOptions
  | StringWithAsyncImporterOptions
) &
  AsyncPlugins;

interface SharedOptions {
  style?: OutputStyle;
  // loadPaths?: string[];
  logger?: Logger;
  color?: boolean;
  quietDeps?: boolean;
  verbose?: boolean;
  charset?: boolean;
  sourceMap?: boolean;
  includedUrls?: boolean;
}

interface Plugins {
  // TODO(awjin): functions: Callable[];
  // TODO(awjin): importers: Importer[];
}

interface AsyncPlugins {
  // TODO(awjin): functions: Array<AsyncCallable | Callable>;
  // TODO(awjin): importers: Array<AsyncImporter | Importer>;
}

type StringOptions = SharedOptions & {
  syntax?: Syntax;
};

type StringWithImporterOptions = StringOptions & {
  // TODO(awjin): importer: Importer;
  url: string;
};

type StringWithAsyncImporterOptions = StringOptions & {
  // TODO(awjin): importer: AsyncImporter | Importer;
  url: string;
};

export enum OutputStyle {
  EXPANDED = 0,
  COMPRESSED = 1,
  NESTED = 2,
  COMPACT = 3,
}

export enum Syntax {
  SCSS = 0,
  SASS = 1,
  CSS = 2,
}

export interface Logger {
  debug: (message: string, span: SourceSpan) => void;

  warn: (
    message: string,
    span?: SourceSpan,
    trace?: string,
    deprecation?: boolean
  ) => void;
}

export interface SourceSpan {
  text: string;
  start: SourceLocation;
  end?: SourceLocation;
  url?: string;
  context?: string;
}

export interface SourceLocation {
  offset: number;
  line: number;
  column: number;
}
