/**
 * # JavaScript API
 *
 * Sass implementations that are available for use via JavaScript must expose
 * the following JavaScript API. As with the rest of this specification, they
 * must not add custom extensions that aren't shared across all implementations.
 *
 * > Having a shared, consistent API makes it easy for users to move between
 * > Sass implementations with minimal disruption, and for build system plugins
 * > to seamlessly work with multiple implementations.
 *
 * The JS API is specified as a TypeScript type declaration. Implementations
 * must adhere to this declaration and to the behavioral specifications written
 * in JSDoc comments on the declarations. Implementations may throw errors when
 * user code passes in values that don't adhere to the type declaration, but
 * unless otherwise indicated they may also handle these values in undefined
 * ways in accordance with the common JavaScript pattern of avoiding explicit
 * type checks. This must not be used as a way of adding custom extensions that
 * aren't shared across all implementations.
 *
 * Certain interfaces in the JS API are defined within the `legacy` directory,
 * indicating that they're part of the legacy Node Sass API. This API is
 * deprecated and implementations are not required to support it. However, at
 * least partial support is recommended for compatibility with older
 * applications and particularly build system plugins.
 *
 * As with other sections of this specification, the specification of the legacy
 * JS API is incomplete, and is added to *lazily*. This means that portions of
 * the spec—particularly the documentation comments that serve as a behavioral
 * specification—are only written when they're necessary as background for new
 * API proposals. */

export {
  CompileResult,
  compile,
  compileAsync,
  compileString,
  compileStringAsync,
} from './compile';
export {Exception} from './exception';
export {FileImporter, Importer, ImporterResult} from './importer';
export {Logger, SourceSpan, SourceLocation} from './logger';
export {
  CustomFunction,
  Options,
  OutputStyle,
  StringOptions,
  StringOptionsWithImporter,
  StringOptionsWithoutImporter,
  Syntax,
} from './options';
export {PromiseOr} from './util/promise_or';
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
  FALSE,
  LegacyAsyncFunction,
  LegacyAsyncFunctionDone,
  LegacyFunction,
  LegacySyncFunction,
  LegacyValue,
  NULL,
  TRUE,
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
 * Information about the Sass implementation. This must begin with a unique
 * identifier for this package (typically but not necessarily the npm package
 * name), followed by U+0009 TAB, followed by its npm package version. It may
 * contain another tab character followed by additional information, but this is
 * not required.
 */
export const info: string;
