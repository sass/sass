import {LegacyPluginThis} from './plugin_this';

interface LegacyImporterThis extends LegacyPluginThis {
  fromImport: boolean;
}

type LegacySyncImporter = (
  this: LegacyImporterThis,
  url: string,
  prev: string
) => {file: string} | {contents: string};

type LegacyAsyncImporter = (
  this: LegacyImporterThis,
  url: string,
  prev: string,
  done: (data: {file: string} | {contents: string} | Error) => void
) => void;

export type LegacyImporter<sync = 'sync' | 'async'> = sync extends 'async'
  ? LegacySyncImporter | LegacyAsyncImporter
  : LegacySyncImporter;
