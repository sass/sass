import {PluginThis} from './plugin_this';

interface ImporterThis extends PluginThis {
  fromImport: boolean;
}

type _SyncImporter = (
  this: ImporterThis,
  url: string,
  prev: string
) => {file: string} | {contents: string};

type _AsyncImporter = (
  this: ImporterThis,
  url: string,
  prev: string,
  done: (data: {file: string} | {contents: string} | Error) => void
) => void;

export type LegacyImporter<sync = 'sync' | 'async'> =
  | _SyncImporter
  | (sync extends 'async' ? _AsyncImporter : never);
