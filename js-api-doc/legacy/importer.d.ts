import {PluginThis} from './plugin_this';

interface ImporterThis extends PluginThis {
  fromImport: boolean;
}

type LegacySyncImporter = (
  this: ImporterThis,
  url: string,
  prev: string
) => {file: string} | {contents: string};

type LegacyAsyncImporter = (
  this: ImporterThis,
  url: string,
  prev: string,
  done: (data: {file: string} | {contents: string} | Error) => void
) => void;

export type LegacyImporter<sync = 'sync' | 'async'> = sync extends 'async'
  ? LegacySyncImporter | LegacyAsyncImporter
  : LegacySyncImporter;
