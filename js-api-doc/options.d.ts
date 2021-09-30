import {URL} from 'url';

import {FileImporter, Importer} from './importer';
import {Logger} from './logger';
import {Value} from './value';
import {PromiseOr} from './util/promise_or';

export type Syntax = 'scss' | 'indented' | 'css';

export type OutputStyle = 'expanded' | 'compressed';

export type CustomFunction<sync extends 'sync' | 'async'> = (
  args: Value[]
) => PromiseOr<Value, sync>;

export interface Options<sync extends 'sync' | 'async'> {
  alertAscii?: boolean;

  alertColor?: boolean;

  functions?: Record<string, CustomFunction<sync>>;

  importers?: (Importer<sync> | FileImporter<sync>)[];

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
        importer: Importer<sync> | FileImporter<sync>;
        url: URL;
      }
  );
