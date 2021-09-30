import {Logger} from '../logger';
import {LegacyImporter} from './importer';
import {LegacyFunction} from './function';

/**
 *
 * This is only exported so that it can be modified by proposals for new
 * features. It should not be referred to by user code.
 */
export interface _Options<sync = 'sync' | 'async'> {
  includePaths?: string[];
  indentedSyntax?: boolean;
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

  charset?: boolean;

  quietDeps?: boolean;

  verbose?: boolean;

  logger?: Logger;
}

export type LegacyOptions<sync = 'sync' | 'async'> = _Options<sync> &
  (
    | {
        file: string;
      }
    | {
        data: string;
        file?: string;
      }
  );
