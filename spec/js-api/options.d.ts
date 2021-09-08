import {URL} from 'url';

import {FileImporter, Importer} from './importer';
import {Logger} from './logger';
import {Value} from './value';

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
export type CustomFunction<sync extends 'sync' | 'async'> = sync extends 'sync'
  ? (args: Value[]) => Value
  : (args: Value[]) => Value | Promise<Value>;

/**
 * > This type allows options to refer to tersely refer to everything that
 * > represents an importer.
 */
type _Importer<sync extends 'sync' | 'async'> =
  | Importer<sync>
  | FileImporter<sync>;

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
   * errors and logs that aren't handled by a `logger`.
   *
   * > The specific format can vary from implementation to implementation.
   * > Compilers are not obligated to use terminal colors if they are irrelevant
   * > in the output environment.
   *
   * @default false
   */
  alertColor?: boolean;

  /**
   * When the compiler encounters a global function call with a signature that
   * does not match that of a built-in function, but matches a key in this map,
   * it must call the associated `CustomFunction` and return its result.
   *
   * The compiler must throw an error if the `CustomFunction` does not return a
   * `Value`.
   */
  functions?: Record<string, CustomFunction<sync>>;

  /** The list of of custom importers to use to resolve file loads. */
  importers?: _Importer<sync>[];

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

/** Additional options specific to compiling from a string. */
export type StringOptions<sync extends 'sync' | 'async'> = Options<sync> & {
  /**
   * The compiler must parse `source` using this syntax.
   *
   * @default 'scss'
   */
  syntax?: Syntax;
} & (
    | {
        /** The canonical URL of the entrypoint. */
        url?: URL;
      }
    | {
        /** The importer to use to resolve relative imports in the entrypoint. */
        importer: _Importer<sync>;
        /** The canonical URL of the entrypoint. */
        url: URL;
      }
  );
