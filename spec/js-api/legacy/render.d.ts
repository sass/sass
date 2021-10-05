import {LegacyException} from './exception';
import {LegacyOptions} from './options';

export interface LegacyResult {
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

export function renderSync(options: LegacyOptions<'sync'>): LegacyResult;

export function render(
  options: LegacyOptions<'async'>,
  callback: (exception?: LegacyException, result?: LegacyResult) => void
): void;
