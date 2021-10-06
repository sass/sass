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
export {FileImporter, Importer, ImporterResult} from './importer';
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
  LegacyAsyncImporter,
  LegacyImporter,
  LegacyImporterResult,
  LegacyImporterThis,
  LegacySyncImporter,
} from './legacy/importer';
export {
  LegacySharedOptions,
  LegacyFileOptions,
  LegacyStringOptions,
  LegacyOptions,
} from './legacy/options';
export {LegacyPluginThis} from './legacy/plugin_this';
export {LegacyResult, render, renderSync} from './legacy/render';

/**
 * Information about the Sass implementation. This always begins with a unique
 * identifier for the Sass implementation, followed by U+0009 TAB, followed by
 * its npm package version. Some implementations include additional information
 * as well, but not in any standardized format.
 *
 * * For Dart Sass, the implementation name is `dart-sass`.
 * * For Node Sass, the implementation name is `node-sass`.
 * * For the embedded host, the implementation name is `sass-embedded`.
 */
export const info: string;
