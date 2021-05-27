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
 * they may also handle these values in undefined ways in accordance with the
 * common JavaScript pattern of avoiding explicit type checks. This must not be
 * used as a way of adding custom extensions that aren't shared across all
 * implementations.
 *
 * As with other sections of this specification, the specification of the JS API
 * is incomplete, and is added to *lazily*. This means that portions of the
 * spec—particularly the documentation comments that serve as a behavioral
 * specification—are only written when they're necessary as background for new
 * API proposals.
 *
 * ## API
 */

/**
 * An extra set of properties passed to the Logger.debug method
 */
interface DebugData {
  /**
   * A set of information related to the location of the error
   */
  source: SourceSpan;
}

/**
 * Required options when compiling a SASS file to CSS data
 */
interface FileOptions extends SharedOptions {
  /**
   * The path of the SASS file to compile
   */
  file: string;
}

/**
 * The shared interface for the `this` keyword for custom importers and custom
 * functions. The implementation must invoke importers and custom functions with
 * an appropriate `this`.
 */
interface PluginThis {
  options: {
    /** The `file` option passed to the `render()` or `renderSync()` call. */
    file?: string;

    /** The `data` option passed to the `render()` or `renderSync()` call. */
    data?: string;

    /**
     * A string that contains the current working directory followed by strings
     * passed in the `includePaths` option, separated by `";"` on Windows and
     * `":"` elsewhere.
     */
    includePaths: string;

    precision: 10;

    /**
     * An integer. The specific semantics of this are left up to the
     * implementation. (The reference implementation always returns 1.)
     */
    style: number;

    /**
     * The number 1 if the `indentType` option was `tab`. The number 0
     * otherwise.
     */
    indentType: 1 | 0;

    /**
     * An integer indicating the number of spaces or tabs emitted by the
     * compiler for each level of indentation.
     */
    indentWidth: number;

    /**
     * The `linefeed` option passed to the `render()` or `renderSync()`, or
     * `'lf'` if no value was passed.
     */
    linefeed: 'cr' | 'crlf' | 'lf' | 'lfcr';

    result: {
      stats: {
        /**
         * The number of milliseconds since the Unix epoch (1 January 1970
         * 00:00:00 UT) at the point at which the user called `render()` or
         * `renderSync()`.
         */
        start: number;

        /**
         * The `file` option passed to the `render()` call, or the string
         * `"data"` if no file was passed.
         */
        entry: string;
      };
    };
  };
}

/**
 * The interface for the `this` keyword for custom importers. The implementation
 * must invoke importers with an appropriate `this`.
 */
interface ImporterThis extends PluginThis {
  /**
   * `true` if this importer invocation was caused by an `@import` statement and
   * `false` otherwise.
   *
   * > This allows importers to look for `.import.scss` stylesheets if and only
   * > if an `@import` is being resolved.
   */
  fromImport: boolean;
}

/**
 * An importer function to customsise how imports are handled
 */
type Importer = (
  this: ImporterThis,

  /**
   * The import path as-is in the file. The path is not resolved.
   */
  url: string,

  /**
   * The previously resolved path
   */
  prev: string
) => {file: string} | {contents: string};

/**
 * An asyncronous importer function to customsise how imports are handled
 */
type ImporterAsync = (
  this: ImporterThis,

  /**
   * The import path as-is in the file. The path is not resolved.
   */
  url: string,

  /**
   * The previously resolved path
   */
  prev: string,

  /**
   * The callback to use once the custom handling has completed
   */
  done: (data: {file: string} | {contents: string} | Error) => void
) => void;

/**
 * A set of custom functions to handle any warnings or debug messages
 */
interface Logger {
  /**
   * A function that is called for each warning - including `@warn` statements
   * @param data All data relative to the warning messagae
   *
   * @default undefined
   */
  warn?(message: string, data: WarnData): void;

  /**
   * A function that is called each time a `@debug` statement is encountered
   * @param data All data relative to the `@debug` statement
   *
   * @default undefined
   */
  debug?(message: string, data: DebugData): void;
}

/**
 * A set of options to pass to the `render()` call
 */
interface Options extends SharedOptions {
  /**
   * Handles when the `@import` directive is encountered.
   *
   * A custom importer allows extension of the sass engine in both a synchronous and asynchronous manner.
   *
   * @default undefined
   */
  importer?: Importer | Importer[];

  /**
   * Holds a collection of custom functions that may be invoked by the sass files being compiled.
   *
   * @default undefined
   */
  functions?: SassFunction;
}

/**
 * A set of options to pass to the `render()` call
 */
interface OptionsAsync extends SharedOptions {
  /**
   * The fibers package to help improve the compilation time
   *
   * @default null
   */
  fibers?: unknown;

  /**
   * Handles when the `@import` directive is encountered.
   *
   * A custom importer allows extension of the sass engine in both a synchronous and asynchronous manner.
   *
   * @default undefined
   */
  importer?: ImporterAsync | ImporterAsync[];

  /**
   * Holds a collection of custom functions that may be invoked by the sass files being compiled.
   *
   * @default undefined
   */
  functions?: SassFunctionAsync;
}

/**
 * The result of the SASS compilation
 */
interface Result {
  /**
   * The compiled CSS.
   *
   * Write this to a file, or serve it out as needed.
   */
  css: Buffer;

  /**
   * The source map.
   *
   * Write this to a file, or serve it out as needed.
   *
   * @default null
   */
  map?: Buffer;

  /**
   * The statistics of the compilation process
   */
  stats: {
    /**
     * The path to the scss file, or `data` if the source was not a file.
     */
    entry: string;

    /**
     * `Date.now()` before the compilation.
     */
    start: number;

    /**
     * `Date.now()` after the compilation.
     */
    end: number;

    /**
     * `end - start`
     */
    duration: number;

    /**
     * Absolute paths to all related files in no particular order.
     */
    includedFiles: string[];
  };
}

/**
 * Convert SASS to CSS asyncronously
 * @param options A set of options for the SASS compiler
 * @param callback The callback to handle the output of the call
 */
export function render(
  options: RenderOptionsAsync,
  callback: (exception: SassException, result: Result) => void
): void;

/**
 * Options that are passed to render().
 */
export type RenderOptionsAsync = (FileOptions | StringOptions) & OptionsAsync;

/**
 * Convert SASS to CSS
 * @param options A set of options for the SASS compiler
 *
 * @throws {SassException}
 */
export function renderSync(options: RenderOptions): Result;

/**
 * Options that are passed to renderSync().
 */
export type RenderOptions = (FileOptions | StringOptions) & Options;

/**
 * Required options when compiling a SASS string to CSS data
 */
interface StringOptions extends SharedOptions {
  /**
   * A string to pass to compile.
   *
   * It is recommended that you use `includePaths` in conjunction with this so that sass can find files when using the `@import` directive.
   */
  data: string;

  /**
   * The path of the SASS file to compile
   *
   * @default null
   */
  file?: string;
}

/**
 * A set of custom functions that are called whenever it's `key` is found during compilation
 */
interface SassFunction {
  [key: string]: (
    this: PluginThis,
    ...args: types.SassType[]
  ) => types.SassType;
}

/**
 * A set of custom asyncronous functions that are called whenever it's `key` is found during compilation
 */
interface SassFunctionAsync {
  [key: string]: <T extends types.SassType[]>(
    this: PluginThis,
    ...args: [...T, (type: types.SassType) => void]
  ) => void;
}

/**
 * Required options when compiling SASS to CSS data
 */
interface SharedOptions {
  /**
   * An array of paths that should be looked in to attempt to resolve your `@import` declarations.
   * When using `data`, it is recommended that you use this.
   *
   * @default []
   */
  includePaths?: string[];

  /**
   * Enable Sass Indented Syntax for parsing the data string or file.
   *
   * @default false
   */
  indentedSyntax?: boolean;

  /**
   * Used to determine whether to use space or tab character for indentation.
   *
   * @default 'space'
   */
  indentType?: 'space' | 'tab';

  /**
   * Used to determine the number of spaces or tabs to be used for indentation.
   *
   * @default 2
   */
  indentWidth?: number;

  /**
   * Used to determine which sequence to use for line breaks.
   *
   * @default 'lf'
   */
  linefeed?: 'cr' | 'crlf' | 'lf' | 'lfcr';

  /**
   * Disable the inclusion of source map information in the output file.
   *
   * @default false
   */
  omitSourceMapUrl?: boolean;

  /**
   * Specify the intended location of the output file.
   * Strongly recommended when outputting source maps so that they can properly refer back to their intended files.
   *
   * @default null
   */
  outFile?: string;

  /**
   * Determines the output format of the final CSS style.
   *
   * @default 'expanded'
   */
  outputStyle?: 'compressed' | 'expanded';

  /**
   * Enables the outputting of a source map.
   *
   * @default undefined
   */
  sourceMap?: boolean | string;

  /**
   * Includes the contents in the source map information.
   *
   * @default false
   */
  sourceMapContents?: boolean;

  /**
   * Embeds the source map as a data URI.
   *
   * @default false
   */
  sourceMapEmbed?: boolean;

  /**
   * The value will be emitted as `sourceRoot` in the source map information.
   *
   * @default undefined
   */
  sourceMapRoot?: string;

  /**
   * Callbacks for handling any `@warn` or `@debug` messages
   *
   * @default undefined
   */
  logger?: Logger;
}

/**
 * Error details of the SASS compilation failure
 */
interface SassException extends Error {
  /**
   * The error message.
   */
  message: string;

  /**
   * The formatted error.
   */
  formatted: string;

  /**
   * The line number of the error.
   */
  line: number;

  /**
   * The column number of error.
   */
  column: number;

  /**
   * The status code.
   */
  status: number;

  /**
   * The filename of error.
   *
   * In case `file` option was not set (in favour of `data`), this will reflect the value `stdin`.
   */
  file: string;
}

/**
 * Location data for a specific location in a file
 */
interface SourceLocation {
  /**
   * The 0-indexed offset of the location
   */
  offset: number;

  /**
   * The line number of the location
   */
  line: number;

  /**
   * The 0-indexed column of the location
   */
  column: number;
}

/**
 * Location information for a warning
 */
interface SourceSpan {
  /**
   * The text between `start` and `end` locations.
   *
   * This is from the exact line and column
   */
  text: string;

  /**
   * The start location of the `@warn` or `@debug` message
   */
  start: SourceLocation;

  /**
   * The end location of the `@warn` or `@debug` message
   */
  end: SourceLocation;

  /**
   * The location of the file with the message
   */
  url: string;

  /**
   * The text from `start` to `end`.
   *
   * However, unlike `text`, it outputs each line in full
   */
  context: string;
}

/**
 * A set of SASS types that are passed to the `function` option
 */
export namespace types {
  /**
   * Root abstraction of a SassType, this can be of any type (Null, Number, String, etc.)
   */
  abstract class SassType {}

  /**
   * A null
   */
  export class Null extends SassType {
    static NULL: Null;
  }

  /**
   * A number
   */
  export class Number implements SassType {
    constructor(value: number, unit?: string);
    getValue(): number;
    setValue(value: number): void;
    getUnit(): string;
    setUnit(unit: string): void;
  }

  /**
   * A string
   */
  export class String implements SassType {
    constructor(value: string);
    getValue(): string;
    setValue(value: string): void;
  }

  /**
   * A boolean
   */
  export class Boolean<T extends boolean = boolean> implements SassType {
    constructor(value: T);
    getValue(): T;
    static readonly TRUE: Boolean<true>;
    static readonly FALSE: Boolean<false>;
  }

  /**
   * A colour with Hue, Saturation & Lightness values
   */
  export class HslColor implements SassType {
    constructor(h: number, s: number, l: number, a?: number);
    getH(): number;
    setH(value: number): void;
    getS(): number;
    setS(value: number): void;
    getL(): number;
    setL(value: number): void;
    getA(): number;
    setA(value: number): void;
  }

  /**
   * A colour with Red, Green & Blue values
   */
  export class RgbColor implements SassType {
    constructor(r: number, g: number, b: number, a?: number);
    getR(): number;
    setR(value: number): void;
    getG(): number;
    setG(value: number): void;
    getB(): number;
    setB(value: number): void;
    getA(): number;
    setA(value: number): void;
  }

  /**
   * A List of data
   */
  export class List<T extends SassType = SassType> implements SassType {
    constructor(length: number, commaSeparator?: boolean);
    getValue(index: number): T | undefined;
    setValue(index: number, value: T): void;
    getSeparator(): boolean;
    setSeparator(isComma: boolean): void;
    getLength(): number;
  }

  /**
   * A map of key-value data
   */
  export class Map<K extends SassType = SassType, V extends SassType = SassType>
    implements SassType
  {
    constructor(length: number);
    getValue(index: number): V;
    setValue(index: number, value: V): void;
    getKey(index: number): K;
    setKey(index: number, key: K): void;
    getLength(): number;
  }
}

/**
 * An extra set of properties passed to the Logger.warn method
 */
interface WarnData {
  /**
   * A set of information related to the location of the error
   *
   * @default undefined
   */
  source?: SourceSpan;

  /**
   * Is it a deprecation message
   */
  deprecation: boolean;

  /**
   * The stack trace of the warning
   */
  stack?: string;
}
