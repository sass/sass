import {FileImporter, Importer} from './importer';
import {Logger} from './logger';
import {Value} from './value';
import {PromiseOr} from './util/promise_or';

/** The types of input syntax that the compiler can parse. */
export type Syntax = 'scss' | 'indented' | 'css';

/**
 * The ways in which the compiler can format the emitted CSS.
 *
 * > The specifics of each format can vary from implementation to
 * > implementation. If an implementation wants to add a new OutputStyle, they
 * > should expand this type.
 */
export type OutputStyle = 'expanded' | 'compressed';

/** A custom function that can be called from Sass stylesheets. */
export type CustomFunction<sync extends 'sync' | 'async'> = (
  args: Value[]
) => PromiseOr<Value, sync>;

/**
 * All of the options for a Sass compilation that are shared by compiling from a
 * path and by compiling from a string.
 */
export interface Options<sync extends 'sync' | 'async'> {
  /**
   * If true, the compiler must use only ASCII characters in the formatted
   * message of errors and logs that aren't handled by a `logger`.
   *
   * @default false
   */
  alertAscii?: boolean;

  /**
   * If true, the compiler may use terminal colors in the formatted message of
   * errors and logs that aren't handled by a `logger`. Implementations may
   * choose the default value for this based on their own heuristics of whether
   * colored output would be useful or render appropriately. Implementations are
   * not obligated to use colors even if this is `true`.
   *
   * > The specific format can vary from implementation to implementation.
   */
  alertColor?: boolean;

  /**
   * When the compiler encounters a global function call with a signature that
   * does not match that of a built-in function, but matches a key in this
   * record, it must call the associated `CustomFunction` and return its result.
   * If the function throws an error or returns anything other than a `Value`,
   * the compiler should treat it as the Sass function throwing an error.
   *
   * > As in the rest of Sass, `_`s and `-`s are considered equivalent when
   * > determining which function signatures match.
   *
   * Before beginning compilation, if any key in this record is not an
   * [<ident-token>] followed immediately by an `ArgumentDeclaration`, the
   * compiler must throw an error.
   *
   * [<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
   *
   * If the `CustomFunction` returns an invalid value, or a value that
   * transitively contains an invalid value, the compiler must treat that as the
   * Sass function throwing an error. The following values are considered
   * invalid:
   *
   * - An object that's not an instance of the `Value` class.
   *
   * - A `SassFunction` whose `signature` field isn't a valid Sass function
   *   signature that could appear after the `@function` directive in a Sass
   *   stylesheet.
   */
  functions?: Record<string, CustomFunction<sync>>;

  /** The list of of custom importers to use to resolve file loads. */
  importers?: (Importer<sync> | FileImporter<sync>)[];

  /** If set, the compiler must use these paths to resolve imports. */
  loadPaths?: string[];

  /**
   * An object that provides callbacks for the compiler to use in lieu of its
   * default messaging behavior.
   *
   * The compiler must treat an `undefined` logger identically to an object
   * that doesn't have `warn` or `debug` fields.
   */
  logger?: Logger;

  /**
   * If true, the compiler must not print deprecation warnings for stylesheets
   * that are transitively loaded through an import path.
   *
   * @default false
   */
  quietDeps?: boolean;

  /**
   * If true, the compiler must set the sourceMap field of the `CompileResult`
   * to a sourceMap object.
   *
   * @default false
   */
  sourceMap?: boolean;

  /**
   * If true, the compiler must include the sources in the sourceMap of the
   * `CompileResult`.
   *
   * @default false
   */
  sourceMapIncludeSources?: boolean;

  /**
   * If present, the compiler must format the emitted CSS in this style.
   *
   * Implementations may support any amount of options, provided that:
   * - They support the 'expanded' option.
   * - They produce CSS that is semantically equivalent regardless of style.
   * - They throw an error if they receive a value for this option that they
   *   do not support.
   */
  style?: OutputStyle;

  /**
   * If `true`, the compiler must print every single deprecation warning it
   * encounters.
   *
   * If `false`, the compiler may choose not to print repeated deprecation
   * warnings.
   *
   * @default false
   */
  verbose?: boolean;
}

export interface StringOptionsWithoutImporter<sync extends 'sync' | 'async'>
  extends Options<sync> {
  /**
   * The compiler must parse `source` using this syntax.
   *
   * @default 'scss'
   */
  syntax?: Syntax;

  /** When `importer` isn't passed, this is purely advisory. */
  url?: URL;
}

export interface StringOptionsWithImporter<sync extends 'sync' | 'async'>
  extends StringOptionsWithoutImporter<sync> {
  /** The importer to use to resolve relative imports in the entrypoint. */
  importer: Importer<sync> | FileImporter<sync>;

  /** The canonical URL of the entrypoint. */
  url: URL;
}

export type StringOptions<sync extends 'sync' | 'async'> =
  | StringOptionsWithImporter<sync>
  | StringOptionsWithoutImporter<sync>;
