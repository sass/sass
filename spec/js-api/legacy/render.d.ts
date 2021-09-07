import {SassException} from './exception';
import {Options} from './options';

export interface Result {
  css: Buffer;
  map?: Buffer;
  stats: {
    entry: string;
    start: number;
    end: number;
    duration: number;
    includedFiles: string[];
  };
}

export function renderSync(options: Options<'sync'>): Result;

export function render(
  options: Options<'async'>,
  callback: (exception: SassException, result: Result) => void
): void;
