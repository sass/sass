// This is a mirror of the JS API definitions in `spec/js-api`, but with comments
// written to provide user-facing documentation rather than to specify behavior for
// implementations.

export {
  CompileResult,
  compile,
  compileAsync,
  compileString,
  compileStringAsync,
} from './compile';
export {Exception} from './exception';
export {
  FileImporter,
  FileImporterResult,
  Importer,
  ImporterResult,
} from './importer';
export {Logger, SourceSpan, SourceLocation} from './logger';
export {
  CustomFunction,
  Options,
  OutputStyle,
  StringOptions,
  Syntax,
} from './options';
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
export {LegacyFunction, LegacyValue, types} from './legacy/function';
export {LegacyImporter} from './legacy/importer';
export {LegacyOptions} from './legacy/options';
export {LegacyResult, render, renderSync} from './legacy/render';

export const info: string;
