// This is a mirror of the JS API definitions in `spec/js-api`, but with comments
// written to provide user-facing documentation rather than to specify behavior for
// implementations.

/** @hidden */
export {
  CompileResult,
  compile,
  compileAsync,
  compileString,
  compileStringAsync,
} from './compile';
/** @hidden */
export {Exception} from './exception';
/** @hidden */
export {
  FileImporter,
  FileImporterResult,
  Importer,
  ImporterResult,
} from './importer';
export {Logger, SourceSpan, SourceLocation} from './logger';
/** @hidden */
export {
  CustomFunction,
  Options,
  OutputStyle,
  StringOptions,
  Syntax,
} from './options';
/** @hidden */
export {PromiseOr} from './util/promise_or';
/** @hidden */
export {
  ListSeparator,
  SassArgumentList,
  SassBoolean,
  SassColor,
  SassFunction,
  SassList,
  SassMap,
  SassNumber,
  SassString,
  Value,
  sassFalse,
  sassNull,
  sassTrue,
} from './value';

// Legacy APIs
export {LegacyException} from './legacy/exception';
export {
  LegacyAsyncFunction,
  LegacySyncFunction,
  LegacyFunction,
  LegacyValue,
  types,
} from './legacy/function';
export {
  LegacyImporterThis,
  LegacyAsyncImporter,
  LegacySyncImporter,
  LegacyImporter,
} from './legacy/importer';
export {
  LegacySharedOptions,
  LegacyFileOptions,
  LegacyStringOptions,
  LegacyOptions,
} from './legacy/options';
export {LegacyPluginThis} from './legacy/plugin_this';
export {LegacyResult, render, renderSync} from './legacy/render';

export const info: string;
