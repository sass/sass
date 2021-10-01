import {URL} from 'url';

import {FileImporter, Importer} from './importer';
import {Logger} from './logger';
import {Value} from './value';

export type Syntax = 'scss' | 'indented' | 'css';

export type OutputStyle = 'expanded' | 'compressed';

export type CustomFunction<sync extends 'sync' | 'async'> = sync extends 'sync'
  ? (args: Value[]) => Value
  : (args: Value[]) => Value | Promise<Value>;

type _Importer<sync extends 'sync' | 'async'> =
  | Importer<sync>
  | FileImporter<sync>;

export interface Options<sync extends 'sync' | 'async'> {
  alertAscii?: boolean;

  alertColor?: boolean;

  functions?: Record<string, CustomFunction<sync>>;

  importers?: _Importer<sync>[];

  loadPaths?: string[];

  logger?: Logger;

  quietDeps?: boolean;

  sourceMap?: boolean;

  style?: OutputStyle;

  verbose?: boolean;
}

export type StringOptions<sync extends 'sync' | 'async'> = Options<sync> & {
  syntax?: Syntax;
} & (
    | {
        url?: URL;
      }
    | {
        importer: _Importer<sync>;
        url: URL;
      }
  );
