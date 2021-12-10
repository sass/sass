import {Logger} from '../logger';
import {LegacyImporter} from './importer';
import {LegacyFunction} from './function';

/**
 * All the options for a Sass compilation except those that specify the specific
 * input format.
 */
export interface LegacySharedOptions<sync extends 'sync' | 'async'> {
  includePaths?: string[];

  indentType?: 'space' | 'tab';

  indentWidth?: number;

  linefeed?: 'cr' | 'crlf' | 'lf' | 'lfcr';

  omitSourceMapUrl?: boolean;

  outFile?: string;

  outputStyle?: 'compressed' | 'expanded' | 'nested' | 'compact';

  sourceMap?: boolean | string;

  sourceMapContents?: boolean;

  sourceMapEmbed?: boolean;

  sourceMapRoot?: string;

  importer?: LegacyImporter<sync> | LegacyImporter<sync>[];

  functions?: {[key: string]: LegacyFunction<sync>};

  /**
   * If `true`, the compiler may prepend `@charset "UTF-8";` or U+FEFF
   * (byte-order marker) if it outputs non-ASCII CSS.
   *
   * If `false`, the compiler never emits these byte sequences. This is ideal
   * when concatenating or embedding in HTML `<style>` tags. (The output will
   * still be UTF-8.)
   *
   * @default true
   */
  charset?: boolean;

  /**
   * If true, the compiler must not print deprecation warnings for stylesheets
   * that are transitively loaded through an import path or importer.
   *
   * @default false
   */
  quietDeps?: boolean;

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

  /**
   * An object that provides callbacks for the compiler to use in lieu of its
   * default messaging behavior.
   *
   * The compiler must treat an `undefined` logger identically to an object that
   * doesn't have `warn` or `debug` fields.
   */
  logger?: Logger;
}

export interface LegacyFileOptions<sync extends 'sync' | 'async'>
  extends LegacySharedOptions<sync> {
  file: string;

  data?: never;
}

export interface LegacyStringOptions<sync extends 'sync' | 'async'>
  extends LegacySharedOptions<sync> {
  data: string;

  file?: string;

  indentedSyntax?: boolean;
}

export type LegacyOptions<sync extends 'sync' | 'async'> =
  | LegacyFileOptions<sync>
  | LegacyStringOptions<sync>;
