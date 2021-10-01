import {URL} from 'url';

import {Syntax} from './options';

interface SyncFileImporter {
  findFileUrl(
    url: string,
    options: {fromImport: boolean}
  ): FileImporterResult | null;

  canonicalize?: never;
}

interface AsyncFileImporter {
  findFileUrl(
    url: string,
    options: {fromImport: boolean}
  ): Promise<FileImporterResult | null> | FileImporterResult | null;

  canonicalize?: never;
}

export interface FileImporterResult {
  url: URL;

  sourceMapUrl?: URL;
}

interface SyncImporter {
  canonicalize(url: string, options: {fromImport: boolean}): URL | null;
  load(canonicalUrl: URL): ImporterResult | null;
  findFileUrl?: never;
}

interface AsyncImporter {
  canonicalize(
    url: string,
    options: {fromImport: boolean}
  ): Promise<URL | null> | URL | null;

  load(
    canonicalUrl: URL
  ): Promise<ImporterResult | null> | ImporterResult | null;

  findFileUrl?: never;
}

export interface ImporterResult {
  css: string;

  syntax: Syntax;

  sourceMapUrl?: URL;
}

export type FileImporter<sync extends 'sync' | 'async'> = sync extends 'async'
  ? AsyncFileImporter
  : SyncFileImporter;

export type Importer<sync extends 'sync' | 'async'> = sync extends 'async'
  ? AsyncImporter
  : SyncImporter;
